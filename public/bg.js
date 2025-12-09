
  const canvas = document.getElementById('scene');

  canvas.width = window.innerWidth;
  canvas.height = window.innerHeight;

  const gl = canvas.getContext('webgl2');

  if(!gl)
    throw new Error('webgl2_required');

  gl.clearColor(0, 0, 0, 1);

  // Vertex Shader
  const vsSource = `#version 300 es
    precision highp float;
    layout (location=0) in vec4 position;
    layout (location=1) in vec2 uv;

    out vec2 uv_pos;

    void main() {
      uv_pos = uv;
      gl_Position = position;
    }`;

  // Fragment shader
  const fsSource = `#version 300 es
precision highp float;
precision highp int;

in vec2 uv_pos;
out vec4 fragColor;

uniform uvec4 u_work0;
uniform uvec4 u_work1;
uniform float u_time;

// device orientation (x = roll, y = pitch, z = yaw)
uniform vec3 u_orientation;

// --- simple hash-based 2D noise ---
float hash21(vec2 p) {
  p = fract(p * vec2(123.34, 345.45));
  p += dot(p, p + 34.345);
  return fract(p.x * p.y);
}

// single star layer
//  - scale : density
//  - motion: how this layer drifts
//  - depth : parallax / brightness factor
//  - size  : apparent star size (near = bigger)
float starLayer(vec2 uv, float scale, float time, vec2 motion, float depth, float size) {
  // move this layer over time and with device tilt
  vec2 p = uv * scale + motion;

  // integer cell id and local coords inside the cell
  vec2 cell = floor(p);
  vec2 f    = fract(p) - 0.5;

  // random per cell
  float n = hash21(cell);

  // only some cells contain a star
  float starMask = step(0.985, n);  // ~1.5% of cells

  // soft round star shape; size controls apparent radius
  float r     = length(f);
  float base  = smoothstep(size, 0.0, r);

  // twinkling: much slower now
  float twinkleSpeed = 0.30 + depth * 0.25;        // was ~5.0–8.0
  float twinkle = 0.6 + 0.4 * sin(time * twinkleSpeed + n * 40.0 + (10.*(uv.x+uv.y)));

  // brighter for nearer layers
  float intensity = mix(0.5, 1.4, depth);

  return base * twinkle * starMask * intensity;
}

void main() {
  // uv in [0,1], centered coords for parallax
  vec2 uv = uv_pos;
  vec2 p  = uv * 2.0 - 1.0;

  // boost orientation so stars move more with device rotation
  vec3 ori = vec3(u_orientation.y * 2.5, u_orientation.x * 2.5, u_orientation.z * 2.5);

  // use orientation to drive motion and subtle perspective
  // roll / pitch → horizontal / vertical drift
  vec2 motionBase = vec2(ori.y, -ori.x) * 10.;
  float twist     = ori.z;

  // overall orientation-based brightness tilt (like looking up/down)
  vec3 viewDir = normalize(vec3(p, 1.0));
  vec3 oriDir  = normalize(vec3(ori.xy, 1.0));
  float viewTilt = dot(viewDir, oriDir);
  viewTilt = clamp(viewTilt, -1.0, 1.0);

  // subtle per-frame shimmer from u_work0 (now much weaker)
  vec4 w0 = vec4(u_work0) / 255.0;
  float frameRand = fract(dot(w0, vec4(0.91, 0.37, 0.61, 0.13)));
  float globalShimmer = 0.98 + 0.04 * (frameRand - 0.5); // was 0.85–1.15

  // accumulate several parallax layers
  float stars = 0.0;
  float baseScale = 40.0;

  for (int i = 0; i < 3; i++) {
    float layer = float(i);
    float t     = layer / 2.0; // 0.0, 0.5, 1.0

    // far layers: denser; near layers: sparser, bigger stars
    float scale = baseScale * mix(1.0, 3.5, t);

    // depth: nearer layer = larger depth = stronger parallax + brighter
    float depth = mix(1.0, 0.35, t);

    // apparent star size: near = big, far = tiny
    float size = mix(0.42, 0.06, t);

    // parallax factor linked to depth → big close stars move most
    float parallax = 0.45 * depth; // was 0.12 * depth

    // each layer moves much more with orientation and time now
    vec2 layerMotion = motionBase * (0.35 + 0.45 * layer)
                     + vec2(twist * (0.18 + 0.12 * layer), 0.0);

    // parallax shift using centered coords
    vec2 layerUV = uv + p * parallax;

    stars += starLayer(layerUV, scale, u_time, layerMotion, depth, size);
  }

  // apply overall shimmer and tilt-based modulation
  stars *= globalShimmer;
  stars *= 0.7 + 0.3 * (viewTilt * 0.5 + 0.5);  // keep in a nice range
  stars = clamp(stars, 0.0, 1.0);

  // black background + white stars
  vec3 color = vec3(stars);

  fragColor = vec4(color, 1.0);
}
`;

  // ---- FIXED: more robust shader creation ----
  function createShader(gl, type, source) {
    const shader = gl.createShader(type);
    gl.shaderSource(shader, source);
    gl.compileShader(shader);

    const success = gl.getShaderParameter(shader, gl.COMPILE_STATUS);
    const info = gl.getShaderInfoLog(shader);

    if (!success) {
      console.error('Shader compile error:', info);
      gl.deleteShader(shader);
      throw new Error(info || 'Shader compile error');
    }

    if (info) {
      console.warn('Shader compile log:', info);
    }

    return shader;
  }

  // ---- FIXED: more robust program creation ----
  function createProgram(gl, vertexShader, fragmentShader) {
    const program = gl.createProgram();
    gl.attachShader(program, vertexShader);
    gl.attachShader(program, fragmentShader);
    gl.linkProgram(program);

    const success = gl.getProgramParameter(program, gl.LINK_STATUS);
    const info = gl.getProgramInfoLog(program);

    if (!success) {
      console.error('Program link error:', info);
      gl.deleteProgram(program);
      throw new Error(info || 'Program link error');
    }

    if (info) {
      console.warn('Program link log:', info);
    }

    return program;
  }

  const vertexShader = createShader(gl, gl.VERTEX_SHADER, vsSource);
  const fragmentShader = createShader(gl, gl.FRAGMENT_SHADER, fsSource);

  const program = createProgram(gl, vertexShader, fragmentShader);
  gl.useProgram(program);

  // Vertex Positions
  const positions = new Float32Array([
    -1,-1,0, -1,1,0, 1,1,0,
    1,-1,0, 1,1,0, -1,-1,0
  ]);
  const positionBuffer = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
  gl.bufferData(gl.ARRAY_BUFFER, positions, gl.STATIC_DRAW);
  gl.vertexAttribPointer(0, 3, gl.FLOAT, false, 0, 0);
  gl.enableVertexAttribArray(0);

  // Texture Positions
  const uvPosArray = new Float32Array([
    1,1, 1,0, 0,0,   0,1, 0,0, 1,1
  ]);
  const uvBuffer = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, uvBuffer);
  gl.bufferData(gl.ARRAY_BUFFER, uvPosArray, gl.STATIC_DRAW);
  gl.vertexAttribPointer(1, 2, gl.FLOAT, false, 0, 0);
  gl.enableVertexAttribArray(1);

  const work0Location = gl.getUniformLocation(program, 'u_work0');
  const work1Location = gl.getUniformLocation(program, 'u_work1');
  const timeLocation  = gl.getUniformLocation(program, 'u_time');
  const orientationLocation = gl.getUniformLocation(program, 'u_orientation');

  const work0 = new Uint8Array(4);
  const work1 = new Uint8Array(4);

  // current orientation: [roll, pitch, yaw]
  const orientation = new Float32Array(3);
  let hasDeviceOrientation = false;

  // Device orientation (mobile / tablet)
  if (window.DeviceOrientationEvent) {
    window.addEventListener('deviceorientation', (event) => {
      // event.beta  : [-180, 180] front/back (pitch)
      // event.gamma : [-90,  90]  left/right (roll)
      // event.alpha : [0,    360] compass (yaw)
      const beta  = (event.beta-90  || 0) * Math.PI / 180.0;
      const gamma = (event.gamma || 0) * Math.PI / 180.0;
      const alpha = (event.alpha || 0) * Math.PI / 180.0;

      const roll  = Math.sin(gamma);
      const pitch = Math.sin(beta);
      const yaw   = Math.sin(alpha);

      orientation[0] = roll;
      orientation[1] = pitch;
      orientation[2] = yaw;

      hasDeviceOrientation = true;
    }, true);
  }

  // Mouse position fallback (desktop / sensors unavailable)
  window.addEventListener('mousemove', (event) => {
    // If we're already getting sensor data, don't override it
    if (hasDeviceOrientation) return;

    const x = (event.clientX / window.innerWidth) * 1.0 - 1.0; // [-1, 1]
    const y = (event.clientY / window.innerHeight) * 1.0 - 1.0;

    const roll  = x;        // tilt left/right
    const pitch = -y;       // tilt forward/back
    const yaw   = 0.5 * x;  // subtle twist

    orientation[0] = roll;
    orientation[1] = pitch;
    orientation[2] = yaw;
  });

  function draw(time) {
    window.crypto.getRandomValues(work0);
    window.crypto.getRandomValues(work1);

    gl.uniform4uiv(work0Location, Array.from(work0));
    gl.uniform4uiv(work1Location, Array.from(work1));
    gl.uniform3fv(orientationLocation, orientation);
    gl.uniform1f(timeLocation, time * 0.001);

    gl.clear(gl.COLOR_BUFFER_BIT);
    gl.drawArrays(gl.TRIANGLES, 0, 6);

    window.requestAnimationFrame(draw);
  }

  // Begin generation
  window.requestAnimationFrame(draw);

