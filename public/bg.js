
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
uniform vec3 u_orientation;
uniform float u_ratio; // New uniform for aspect ratio

// --- simple hash-based 2D noise ---
float hash21(vec2 p) {
  p = fract(p * vec2(123.34, 345.45));
  p += dot(p, p + 34.345);
  return fract(p.x * p.y);
}
float hash11(float p) {
  return fract(sin(p * 127.1) * 43758.5453123);
}

// single star layer
float starLayer(vec2 uv, float scale, float time, vec2 motion, float depth, float size) {
  vec2 p = uv * scale + motion;
  vec2 cell = floor(p);
  vec2 f    = fract(p) - 0.5;

  float n = hash21(cell);
  float starMask = step(0.985, n);

  float r     = length(f);
  float base  = smoothstep(size, 0.0, r);

  float twinkleSpeed = 0.30 + depth * 0.25;
  float twinkle = 0.6 + 0.4 * sin(time * twinkleSpeed + n * 40.0 + (10.*(uv.x+uv.y)));
  float intensity = mix(0.5, 1.4, depth);

  return base * twinkle * starMask * intensity;
}

// Tiny moving point + continuous line tail
float shootingStar(vec2 uv, float time) {
    float baseCycle  = 3.;
    float cycleIndex = floor(time / baseCycle);

    float r0     = hash11(cycleIndex);
    float period = mix(3.0, 6.0, r0);
    float phase = fract(time / period);

    if (phase < 0.1) return 0.0;

    float life = (phase - 0.1) / 0.9;
    float movePhase = clamp(life / 0.8, 0.0, 1.0);
    float tailFadePhase = clamp((life - 0.8) / 0.2, 0.0, 1.0);

    float angle = hash11(cycleIndex + 1.0) * 6.2831853;
    vec2 dir = normalize(vec2(cos(angle), sin(angle)));

    // Fix: Spawn range adjusted by aspect ratio so they cover the whole width
    float cx = mix(-0.2 * u_ratio, 1.2 * u_ratio, hash11(cycleIndex + 2.0));
    float cy = mix(-0.2, 1.2, hash11(cycleIndex + 3.0));
    vec2 center = vec2(cx, cy);

    float pathLen = 1.8; // length of path across screen
    float s = (movePhase - 0.5) * pathLen;
    vec2 headPos = center + dir * s;

    // Head
    float coreRadius = mix(0.006, 0.002, hash11(cycleIndex + 2.0));
    float distHead = length(uv - headPos);
    float headBrightBase = smoothstep(coreRadius, 0.0, distHead) * 2.0;

    // Tail
    float tailLen = mix(0.45, 0.2, hash11(cycleIndex + 2.0));
    vec2 toUV = uv - headPos;
    float along = dot(toUV, -dir);
    float withinTail = step(0.0, along) * step(along, tailLen);
    vec2 proj = -dir * along;
    float distLine = length(toUV - proj);

    float tailWidth = mix(0.006, 0.002, hash11(cycleIndex + 2.0));
    float lineCore = smoothstep(tailWidth, 0.0, distLine);
    float alongNorm = clamp(along / tailLen, 0.0, 1.0);
    float tailBrightBase = lineCore * (1.0 - alongNorm) * withinTail;

    float fadeIn = smoothstep(0.0, 0.08, life);
    float headMask = 1.0 - clamp(tailFadePhase * 3.0, 0.0, 1.0);
    float tailMask = 1.0 - tailFadePhase;

    return (headBrightBase * headMask + tailBrightBase * tailMask * 0.5) * fadeIn;
}

void main() {
  // 1. Correct Aspect Ratio
  // We multiply X by the ratio so the grid is square (1 unit X = 1 unit Y)
  // This prevents the stars from looking stretched on wide screens.
  vec2 uvSt = vec2(uv_pos.x * u_ratio, uv_pos.y);

  // Centered coordinates for parallax (also aspect corrected)
  vec2 p = uvSt * 2.0 - vec2(u_ratio, 1.0);

  vec3 ori = vec3(u_orientation.y * 2.5, u_orientation.x * 2.5, u_orientation.z * 2.5);

  vec2 motionBase = vec2(ori.y, -ori.x) * 10.;
  float twist     = ori.z;

  // 2. Add Automatic Idle Drift ("Wind")
  // We add this to the motion so stars move even without device rotation.
  // Speed: 0.05 units per second
  vec2 wind = vec2(u_time * 0.05, u_time * 0.02);

  vec3 viewDir = normalize(vec3(p, 1.0));
  vec3 oriDir  = normalize(vec3(ori.xy, 1.0));
  float viewTilt = clamp(dot(viewDir, oriDir), -1.0, 1.0);

  vec4 w0 = vec4(u_work0) / 255.0;
  float frameRand = fract(dot(w0, vec4(0.91, 0.37, 0.61, 0.13)));
  float globalShimmer = 0.98 + 0.04 * (frameRand - 0.5);

  float stars = 0.0;
  float baseScale = 40.0;

  for (int i = 0; i < 3; i++) {
    float layer = float(i);
    float t     = layer / 2.0;

    float scale = baseScale * mix(1.0, 3.5, t);
    float depth = mix(1.0, 0.35, t);
    float size  = mix(0.42, 0.06, t);
    float parallax = 0.45 * depth;

    // Combine orientation motion with idle wind
    // (depth * wind) makes closer stars drift faster than far ones -> 3D effect
    vec2 combinedMotion = motionBase * (0.35 + 0.45 * layer)
                        + wind * (1.5 - t)
                        + vec2(twist * (0.18 + 0.12 * layer), 0.0);

    vec2 layerUV = uvSt + p * parallax;

    stars += starLayer(layerUV, scale, u_time, combinedMotion, depth, size);
  }

  stars *= globalShimmer;
  stars *= 0.7 + 0.3 * (viewTilt * 0.5 + 0.5);
  stars = clamp(stars, 0.0, 1.0);

  // Pass aspect-corrected UV to shooting star so it is also round
  float shoot = shootingStar(uvSt, u_time);

  vec3 color = vec3(stars + shoot);
  fragColor = vec4(clamp(color, 0.0, 1.0), 1.0);
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
  const ratioLocation = gl.getUniformLocation(program, 'u_ratio');

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
    // Pass width/height so the shader can un-stretch the stars
    gl.uniform1f(ratioLocation, canvas.width / canvas.height);

    gl.clear(gl.COLOR_BUFFER_BIT);
    gl.drawArrays(gl.TRIANGLES, 0, 6);

    window.requestAnimationFrame(draw);
  }

  // Begin generation
  window.requestAnimationFrame(draw);

