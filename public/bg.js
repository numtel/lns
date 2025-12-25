
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
// --- extra hash helpers for shooting star timing/direction ---
float hash11(float p) {
  return fract(sin(p * 127.1) * 43758.5453123);
}

vec2 hash21f(float p) {
  vec2 q = vec2(
    dot(vec2(p, p + 1.23), vec2(127.1, 311.7)),
    dot(vec2(p + 4.56, p + 7.89), vec2(269.5, 183.3))
  );
  return fract(sin(q) * 43758.5453);
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

// Tiny moving point + continuous line tail that lingers and fades out
float shootingStar(vec2 uv, float time) {
    // --- Randomized period per cycle (3–6 seconds) ---
    float baseCycle  = 3.;
    float cycleIndex = floor(time / baseCycle);

    float r0     = hash11(cycleIndex);
    float period = mix(3.0, 6.0, r0);          // [3, 6] seconds

    float phase = fract(time / period);        // 0..1 within this cycle

    // Small initial gap with no star
    if (phase < 0.1) {
        return 0.0;
    }

    // Map remaining 0.1..1.0 to 0..1 "life" of the star
    float life = (phase - 0.1) / 0.9;          // 0..1

    // Head moves only in the first ~80% of life
    float movePhase = clamp(life / 0.8, 0.0, 1.0);

    // Tail fade-out in the last 20% of life
    float tailFadePhase = clamp((life - 0.8) / 0.2, 0.0, 1.0);

    // --- Random direction (full circle) ---
    float angle = hash11(cycleIndex + 1.0) * 6.28318530718; // 0..2π
    vec2 dir = normalize(vec2(cos(angle), sin(angle)));

    // --- Random center point (slightly padded around the screen) ---
    float cx = mix(-0.2, 1.2, hash11(cycleIndex + 2.0));
    float cy = mix(-0.2, 1.2, hash11(cycleIndex + 3.0));
    vec2 center = vec2(cx, cy);

    // Path length large enough to cross the view
    float pathLen = 1.8;
    float s = (movePhase - 0.5) * pathLen;

    // Head position moves along dir
    vec2 headPos = center + dir * s;

    // --- Head: small bright core ---
    float coreRadius = mix(0.006, 0.002, hash11(cycleIndex + 2.0));
    float distHead = length(uv - headPos);
    float headBrightBase = smoothstep(coreRadius, 0.0, distHead) * 2.0;

    // --- Tail: continuous glowing line behind head along -dir ---
    float tailLen = mix(0.45, 0.2, hash11(cycleIndex + 2.0));              // how long the tail is
    vec2 toUV = uv - headPos;

    // Distance along the tail direction (-dir)
    float along = dot(toUV, -dir);

    // Only consider points behind the head and within tail length
    float withinTail = step(0.0, along) * step(along, tailLen);

    // Perpendicular distance to the line
    vec2 proj = -dir * along;
    float distLine = length(toUV - proj);

    // Radial falloff from the line
    float tailWidth = mix(0.006, 0.002, hash11(cycleIndex + 2.0));
    float lineCore = smoothstep(tailWidth, 0.0, distLine);

    // Fade brightness along the line from head (bright) to end (dim)
    float alongNorm = clamp(along / tailLen, 0.0, 1.0);
    float alongFalloff = 1.0 - alongNorm;

    float tailBrightBase = lineCore * alongFalloff * withinTail;

    // --- Temporal behavior: fade in + lingering tail fade out ---

    // Soft fade-in at start of life (for both head & tail)
    float fadeIn = smoothstep(0.0, 0.08, life);

    // Head fades out quickly near the end
    float headMask = 1.0 - clamp(tailFadePhase * 3.0, 0.0, 1.0);  // vanishes fast
    // Tail fades out more slowly over the entire last 20% of life
    float tailMask = 1.0 - tailFadePhase;                         // lingers

    float headBright = headBrightBase * fadeIn * headMask;
    float tailBright = tailBrightBase * fadeIn * tailMask;

    return headBright + tailBright * 0.5;
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

  // add a shooting star every ~3–6 seconds
  float shoot = shootingStar(uv, u_time);

  // black background + white stars + bright shooting star
  vec3 color = vec3(stars + shoot);

  color = clamp(color, 0.0, 1.0);
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
      // 1. Get raw angles in degrees (no Math.abs!)
      const beta  = (event.beta  || 0); // x-axis tilt (front/back)
      const gamma = (event.gamma || 0); // y-axis tilt (left/right)
      const alpha = (event.alpha || 0); // compass direction

      // 2. Handle Screen Rotation (Portrait vs Landscape)
      // We need to swap Beta/Gamma based on how the user is holding the screen.
      const screenAngle = (screen.orientation && screen.orientation.angle) || window.orientation || 0;

      let screenX = 0;
      let screenY = 0;
      const rad = Math.PI / 180.0;

      // Map device hardware axes to screen visual axes
      if (screenAngle === 90) {
        // Landscape Left
        screenX = beta * rad;
        screenY = -gamma * rad;
      } else if (screenAngle === -90 || screenAngle === 270) {
        // Landscape Right
        screenX = -beta * rad;
        screenY = gamma * rad;
      } else if (screenAngle === 180) {
        // Upside Down
        screenX = -gamma * rad;
        screenY = -beta * rad;
      } else {
        // Portrait (0)
        screenX = gamma * rad;
        screenY = beta * rad;
      }

      // 3. Update uniforms
      // We removed Math.abs to fix the "jumping" at axis crossings.
      // We use the re-mapped screenX/Y to fix the "side view" issue.

      orientation[0] = screenX; // Roll (visual X)
      orientation[1] = screenY; // Pitch (visual Y)

      // We dampen or remove Yaw (alpha) because it causes huge jumps
      // when passing North (360->0) and usually feels bad in parallax.
      // Set to 0 or use a very small factor if you really want twist.
      orientation[2] = 0;

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

