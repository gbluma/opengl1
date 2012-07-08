#version 120
uniform sampler2D rnm;
uniform sampler2D normalMap;
varying vec2 uv;

const float totStrength = 1.38;
const float strength = 0.07;
const float offset = 18.0;
const float falloff = 0.000002;
const float rad = 0.006;
const float invSamples = -1.38/10.0;

void main(void)
{
   // these are the random vectors inside a unit sphere
   vec3 pSphere_0 = vec3(-0.010735935, 0.01647018,    0.0062425877);
   vec3 pSphere_1 = vec3(-0.06533369,  0.3647007,    -0.13746321);
   vec3 pSphere_2 = vec3(-0.6539235,  -0.016726388,  -0.53000957);
   vec3 pSphere_3 = vec3( 0.40958285,  0.0052428036, -0.5591124);
   vec3 pSphere_4 = vec3(-0.1465366,   0.09899267,    0.15571679);
   vec3 pSphere_5 = vec3(-0.44122112, -0.5458797,     0.04912532);
   vec3 pSphere_6 = vec3( 0.03755566, -0.10961345,   -0.33040273);
   vec3 pSphere_7 = vec3( 0.019100213, 0.29652783,    0.066237666);
   vec3 pSphere_8 = vec3( 0.8765323,   0.011236004,   0.28265962);
   vec3 pSphere_9 = vec3( 0.29264435, -0.40794238,    0.15964167);
 
   // grab a normal for reflecting the sample rays later on
   vec3 fres = normalize((texture2D(rnm,uv*offset).xyz*2.0) - vec3(1.0));
 
   vec4 currentPixelSample = texture2D(normalMap,uv);
 
   float currentPixelDepth = currentPixelSample.a;
 
   // current fragment coords in screen space
   vec3 ep = vec3(uv.xy,currentPixelDepth);

   // get the normal of current fragment
   vec3 norm = currentPixelSample.xyz;
 
   float bl = 0.0;

   // adjust for the depth ( not shure if this is good..)
   float radD = rad/currentPixelDepth;
 
   //vec3 ray, se, occNorm;
   float occluderDepth, depthDifference;
   vec4 occluderFragment;
   vec3 ray;

    // 0
    ray = radD * reflect(pSphere_0,fres);
    occluderFragment = texture2D(normalMap,ep.xy + sign(dot(ray,norm) )*ray.xy);
    depthDifference = currentPixelDepth-occluderFragment.a;
    bl += step(falloff,depthDifference)*(1.0-dot(occluderFragment.xyz,norm))*(1.0-smoothstep(falloff,strength,depthDifference));
    // 1
    ray = radD * reflect(pSphere_1,fres);
    occluderFragment = texture2D(normalMap,ep.xy + sign(dot(ray,norm) )*ray.xy);
    depthDifference = currentPixelDepth-occluderFragment.a;
    bl += step(falloff,depthDifference)*(1.0-dot(occluderFragment.xyz,norm))*(1.0-smoothstep(falloff,strength,depthDifference));
    // 2
    ray = radD * reflect(pSphere_2,fres);
    occluderFragment = texture2D(normalMap,ep.xy + sign(dot(ray,norm) )*ray.xy);
    depthDifference = currentPixelDepth-occluderFragment.a;
    bl += step(falloff,depthDifference)*(1.0-dot(occluderFragment.xyz,norm))*(1.0-smoothstep(falloff,strength,depthDifference));
    // 3
    ray = radD * reflect(pSphere_3,fres);
    occluderFragment = texture2D(normalMap,ep.xy + sign(dot(ray,norm) )*ray.xy);
    depthDifference = currentPixelDepth-occluderFragment.a;
    bl += step(falloff,depthDifference)*(1.0-dot(occluderFragment.xyz,norm))*(1.0-smoothstep(falloff,strength,depthDifference));
    // 4
    ray = radD * reflect(pSphere_4,fres);
    occluderFragment = texture2D(normalMap,ep.xy + sign(dot(ray,norm) )*ray.xy);
    depthDifference = currentPixelDepth-occluderFragment.a;
    bl += step(falloff,depthDifference)*(1.0-dot(occluderFragment.xyz,norm))*(1.0-smoothstep(falloff,strength,depthDifference));
    // 5
    ray = radD * reflect(pSphere_5,fres);
    occluderFragment = texture2D(normalMap,ep.xy + sign(dot(ray,norm) )*ray.xy);
    depthDifference = currentPixelDepth-occluderFragment.a;
    bl += step(falloff,depthDifference)*(1.0-dot(occluderFragment.xyz,norm))*(1.0-smoothstep(falloff,strength,depthDifference));
    // 6
    ray = radD * reflect(pSphere_6,fres);
    occluderFragment = texture2D(normalMap,ep.xy + sign(dot(ray,norm) )*ray.xy);
    depthDifference = currentPixelDepth-occluderFragment.a;
    bl += step(falloff,depthDifference)*(1.0-dot(occluderFragment.xyz,norm))*(1.0-smoothstep(falloff,strength,depthDifference));
    // 7
    ray = radD * reflect(pSphere_7,fres);
    occluderFragment = texture2D(normalMap,ep.xy + sign(dot(ray,norm) )*ray.xy);
    depthDifference = currentPixelDepth-occluderFragment.a;
    bl += step(falloff,depthDifference)*(1.0-dot(occluderFragment.xyz,norm))*(1.0-smoothstep(falloff,strength,depthDifference));
    // 8
    ray = radD * reflect(pSphere_8,fres);
    occluderFragment = texture2D(normalMap,ep.xy + sign(dot(ray,norm) )*ray.xy);
    depthDifference = currentPixelDepth-occluderFragment.a;
    bl += step(falloff,depthDifference)*(1.0-dot(occluderFragment.xyz,norm))*(1.0-smoothstep(falloff,strength,depthDifference));
    // 9
    ray = radD * reflect(pSphere_9,fres);
    occluderFragment = texture2D(normalMap,ep.xy + sign(dot(ray,norm) )*ray.xy);
    depthDifference = currentPixelDepth-occluderFragment.a;
    bl += step(falloff,depthDifference)*(1.0-dot(occluderFragment.xyz,norm))*(1.0-smoothstep(falloff,strength,depthDifference));

   // output the result
   //vec4 outputColor = vec4( 1.0 + bl * invSamples, 0.0, 0.0, 1.0);
   //gl_FragColor = outputColor;
   gl_FragColor.r = 1.0 + bl * invSamples;

 
}

