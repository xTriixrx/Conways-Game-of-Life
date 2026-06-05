"use strict";

const viewport = document.getElementById("viewport");
const canvas = document.getElementById("world");
const gridCanvas = document.getElementById("grid");
const statusEl = document.getElementById("status");
const playButton = document.getElementById("play-button");
const backButton = document.getElementById("back-button");
const resetViewButton = document.getElementById("reset-view-button");
const ctx = canvas.getContext("2d", { alpha: false });
const gridCtx = gridCanvas.getContext("2d");
const sourceCanvas = document.createElement("canvas");
const sourceCtx = sourceCanvas.getContext("2d", { alpha: false });

let imageData = null;
let dimension = 0;
let lastGeneration = -1;
let lastLiveCount = 0;
let socket = null;
let historyEnabled = false;
let zoom = 1;
let panX = 0;
let panY = 0;
let dragging = false;
let dragStartX = 0;
let dragStartY = 0;
let dragPanX = 0;
let dragPanY = 0;

function readU32(view, offset) {
  return view.getUint32(offset, false);
}

function readU16(view, offset) {
  return view.getUint16(offset, false);
}

function viewportMetrics() {
  const rect = viewport.getBoundingClientRect();
  const pixelRatio = window.devicePixelRatio || 1;
  return {
    rect,
    pixelRatio,
    width: Math.max(1, Math.floor(rect.width * pixelRatio)),
    height: Math.max(1, Math.floor(rect.height * pixelRatio))
  };
}

function visibleSourceSize() {
  const metrics = viewportMetrics();
  const aspect = metrics.height / metrics.width;
  let width = dimension / zoom;
  let height = width * aspect;

  if (height > dimension) {
    height = dimension / zoom;
    width = height / aspect;
  }

  return { width, height };
}

function maxPanX() {
  return Math.max(0, dimension - visibleSourceSize().width);
}

function maxPanY() {
  return Math.max(0, dimension - visibleSourceSize().height);
}

function clampPan() {
  panX = Math.min(maxPanX(), Math.max(0, panX));
  panY = Math.min(maxPanY(), Math.max(0, panY));
}

function resetView() {
  zoom = 1;
  const source = visibleSourceSize();
  panX = Math.max(0, (dimension - source.width) / 2);
  panY = Math.max(0, (dimension - source.height) / 2);
  renderViewport();
}

function resizeDisplayCanvases() {
  const metrics = viewportMetrics();

  if (canvas.width !== metrics.width || canvas.height !== metrics.height) {
    canvas.width = metrics.width;
    canvas.height = metrics.height;
    gridCanvas.width = metrics.width;
    gridCanvas.height = metrics.height;
    ctx.imageSmoothingEnabled = false;
  }
}

function renderGrid(sourceX, sourceY, sourceWidth, sourceHeight) {
  gridCtx.clearRect(0, 0, gridCanvas.width, gridCanvas.height);
  if (dimension === 0) {
    return;
  }

  const cellSize = gridCanvas.width / sourceWidth;
  if (cellSize < 4) {
    return;
  }

  const firstCol = Math.ceil(sourceX);
  const lastCol = Math.floor(sourceX + sourceWidth);
  const firstRow = Math.ceil(sourceY);
  const lastRow = Math.floor(sourceY + sourceHeight);

  gridCtx.save();
  gridCtx.strokeStyle = cellSize >= 10 ? "rgba(32, 33, 36, 0.45)" : "rgba(32, 33, 36, 0.25)";
  gridCtx.lineWidth = Math.max(1, Math.floor(window.devicePixelRatio || 1));
  gridCtx.beginPath();

  for (let col = firstCol; col <= lastCol; col++) {
    const x = Math.round((col - sourceX) * cellSize) + 0.5;
    gridCtx.moveTo(x, 0);
    gridCtx.lineTo(x, gridCanvas.height);
  }

  for (let row = firstRow; row <= lastRow; row++) {
    const y = Math.round((row - sourceY) * cellSize) + 0.5;
    gridCtx.moveTo(0, y);
    gridCtx.lineTo(gridCanvas.width, y);
  }

  gridCtx.stroke();
  gridCtx.restore();
}

function renderViewport() {
  resizeDisplayCanvases();
  ctx.imageSmoothingEnabled = false;

  if (dimension === 0) {
    ctx.fillStyle = "#eceff1";
    ctx.fillRect(0, 0, canvas.width, canvas.height);
    gridCtx.clearRect(0, 0, gridCanvas.width, gridCanvas.height);
    return;
  }

  clampPan();
  const source = visibleSourceSize();
  ctx.clearRect(0, 0, canvas.width, canvas.height);
  ctx.drawImage(
    sourceCanvas,
    panX,
    panY,
    source.width,
    source.height,
    0,
    0,
    canvas.width,
    canvas.height
  );
  renderGrid(panX, panY, source.width, source.height);
  statusEl.textContent = `Generation ${lastGeneration} - ${lastLiveCount} live cells - ${zoom.toFixed(2)}x`;
}

function drawFrame(buffer) {
  const view = new DataView(buffer);
  if (buffer.byteLength < 14 ||
      view.getUint8(0) !== 67 ||
      view.getUint8(1) !== 71 ||
      view.getUint8(2) !== 76 ||
      view.getUint8(3) !== 49) {
    throw new Error("Invalid frame");
  }

  const generation = readU32(view, 4);
  const liveCount = readU32(view, 8);
  const nextDimension = readU16(view, 12);

  if (generation === lastGeneration) {
    return;
  }

  if (nextDimension !== dimension) {
    dimension = nextDimension;
    sourceCanvas.width = dimension;
    sourceCanvas.height = dimension;
    imageData = sourceCtx.createImageData(dimension, dimension);
    resetView();
  }

  const pixels = imageData.data;
  const bytes = new Uint8Array(buffer, 14);
  const cells = dimension * dimension;

  for (let i = 0; i < cells; i++) {
    const alive = (bytes[i >> 3] & (1 << (7 - (i & 7)))) !== 0;
    const offset = i << 2;
    const value = alive ? 0 : 236;
    pixels[offset] = value;
    pixels[offset + 1] = value;
    pixels[offset + 2] = value;
    pixels[offset + 3] = 255;
  }

  sourceCtx.putImageData(imageData, 0, 0);
  lastGeneration = generation;
  lastLiveCount = liveCount;
  renderViewport();
}

function zoomAt(clientX, clientY, factor) {
  if (dimension === 0) {
    return;
  }

  const rect = viewport.getBoundingClientRect();
  const localX = Math.min(rect.width, Math.max(0, clientX - rect.left));
  const localY = Math.min(rect.height, Math.max(0, clientY - rect.top));
  const before = visibleSourceSize();
  const worldX = panX + (localX / rect.width) * before.width;
  const worldY = panY + (localY / rect.height) * before.height;

  zoom = Math.min(128, Math.max(1, zoom * factor));
  const after = visibleSourceSize();
  panX = worldX - (localX / rect.width) * after.width;
  panY = worldY - (localY / rect.height) * after.height;
  renderViewport();
}

function websocketUrl() {
  const protocol = window.location.protocol === "https:" ? "wss" : "ws";
  return `${protocol}://${window.location.host}/ws`;
}

function connectFrameSocket() {
  socket = new WebSocket(websocketUrl());
  socket.binaryType = "arraybuffer";

  socket.addEventListener("open", () => {
    statusEl.textContent = "Connected";
  });

  socket.addEventListener("message", event => {
    if (event.data instanceof ArrayBuffer) {
      drawFrame(event.data);
    }
  });

  socket.addEventListener("close", () => {
    statusEl.textContent = "Connection closed";
  });

  socket.addEventListener("error", () => {
    statusEl.textContent = "WebSocket connection error";
  });
}

async function loadConfig() {
  const response = await fetch("/config", {
    cache: "no-store"
  });
  const config = await response.json();
  historyEnabled = config.historyEnabled;
  backButton.disabled = !historyEnabled;
}

async function sendControl(action) {
  await fetch(`/control?action=${encodeURIComponent(action)}`, {
    cache: "no-store"
  });
  if (action === "resume") {
    playButton.textContent = "Resume";
  }
}

document.querySelectorAll("button[data-action]").forEach(button => {
  button.addEventListener("click", () => sendControl(button.dataset.action));
});

resetViewButton.addEventListener("click", resetView);

viewport.addEventListener("wheel", event => {
  event.preventDefault();
  zoomAt(event.clientX, event.clientY, event.deltaY < 0 ? 1.25 : 0.8);
}, { passive: false });

viewport.addEventListener("pointerdown", event => {
  if (dimension === 0) {
    return;
  }
  dragging = true;
  dragStartX = event.clientX;
  dragStartY = event.clientY;
  dragPanX = panX;
  dragPanY = panY;
  viewport.setPointerCapture(event.pointerId);
});

viewport.addEventListener("pointermove", event => {
  if (!dragging || dimension === 0) {
    return;
  }
  const rect = viewport.getBoundingClientRect();
  const source = visibleSourceSize();
  panX = dragPanX - ((event.clientX - dragStartX) / rect.width) * source.width;
  panY = dragPanY - ((event.clientY - dragStartY) / rect.height) * source.height;
  renderViewport();
});

viewport.addEventListener("pointerup", event => {
  dragging = false;
  viewport.releasePointerCapture(event.pointerId);
});

viewport.addEventListener("pointercancel", () => {
  dragging = false;
});

window.addEventListener("resize", renderViewport);

window.addEventListener("keydown", event => {
  const rect = viewport.getBoundingClientRect();
  const centerX = rect.left + (rect.width / 2);
  const centerY = rect.top + (rect.height / 2);
  if (event.key === "+" || event.key === "=") {
    zoomAt(centerX, centerY, 1.25);
  } else if (event.key === "-" || event.key === "_") {
    zoomAt(centerX, centerY, 0.8);
  } else if (event.key === "0") {
    resetView();
  }
});

window.addEventListener("beforeunload", () => {
  if (socket !== null) {
    socket.close();
  }
});

renderViewport();
loadConfig()
  .then(connectFrameSocket)
  .catch(error => {
    statusEl.textContent = `Connection error: ${error.message}`;
  });
