// glslv output by Cg compiler
// cgc version 3.0.0016, build date Feb 11 2011
// command line args: -profile glslv
// source file: accum.cg
//vendor NVIDIA Corporation
//version 3.0.0.16
//profile glslv
//program main
//semantic main.texture : TEXUNIT0
//semantic main.environmentMap : TEXUNIT1
//semantic main.AccumEnabled
//semantic main.passthrough
//semantic main.intPt
//semantic main.distance
//semantic main.focaldistance
//semantic main.targetThickness
//semantic main.targetThicknessMax
//semantic main.laserEnabled
//semantic main.dotScale
//semantic main.roll
//semantic main.texelAreaCM
//semantic main.surfaceColor
//semantic main.texSize
//semantic main.gloss
//semantic main.specularity
//semantic main.refl
//semantic main.normalBias
//semantic main.max_specularity
//semantic main.max_reflection
//var samplerRECT texture : TEXUNIT0 : _texture1 0 : 1 : 1
//var half AccumEnabled :  : _AccumEnabled1 : 3 : 1
//var half passthrough :  : _passthrough1 : 4 : 1
//var half2 intPt :  : _intPt1 : 5 : 1
//var half distance :  : _distance1 : 6 : 1
//var half focaldistance :  : _focaldistance1 : 7 : 1
//var half targetThickness :  : _targetThickness1 : 8 : 1
//var half targetThicknessMax :  : _targetThicknessMax1 : 9 : 1
//var half laserEnabled :  : _laserEnabled1 : 10 : 1
//var half dotScale :  : _dotScale1 : 11 : 1
//var half roll :  : _roll1 : 12 : 1
//var double texelAreaCM :  : _texelAreaCM1 : 13 : 1
//var half3 surfaceColor :  : _surfaceColor1 : 14 : 1
//var half texSize :  : _texSize1 : 15 : 1
//var float gloss :  : _gloss1 : 16 : 1
//var float refl :  : _refl1 : 18 : 1
//var float normalBias :  : _normalBias1 : 19 : 1
//var float max_reflection :  : _max_reflection1 : 21 : 1
//var float3 IN.texcoord : $vin.TEXCOORD0 : ATTR8 : 0 : 1
//var float4 IN.position : $vin.POSITION :  : 0 : 0
//var float3 IN.positionW : $vin.TEXCOORD4 :  : 0 : 0
//var float3 IN.N : $vin.TEXCOORD5 : ATTR13 : 0 : 1
//var float3 IN.L : $vin.TEXCOORD3 : ATTR11 : 0 : 1
//var float3 IN.V : $vin.TEXCOORD2 : ATTR10 : 0 : 1
//var float3 main.color : $vout.COLOR : COL0 : -1 : 1

#ifdef GL_ARB_texture_rectangle
#extension GL_ARB_texture_rectangle : enable
#endif

varying vec3 _texcoord;
varying vec3 _N;
varying vec3 _L;
varying vec3 _V;

struct pixel_out {
    vec3 _color;
};

float _TMP29;
vec4 _TMP19;
float _TMP34;
float _TMP32;
float _TMP33;
vec3 _TMP17;
float _TMP27;
float _TMP26;
vec3 _TMP16;
vec4 _TMP15;
vec4 _TMP13;
vec4 _TMP12;
vec4 _TMP11;
vec4 _TMP10;
float _TMP9;
float _TMP8;
float _TMP7;
vec4 _TMP6;
float _TMP5;
float _TMP3;
float _TMP4;
float _TMP25;
float _TMP24;
float _TMP23;
float _TMP31;
float _TMP30;
float _TMP2;
float _TMP1;
float _TMP0;
float _TMP22;
float _TMP21;

uniform sampler2DRect _texture1;
uniform float _AccumEnabled1;
uniform float _passthrough1;
uniform vec2 _intPt1;
uniform float _distance1;
uniform float _focaldistance1;
uniform float _targetThickness1;
uniform float _targetThicknessMax1;
uniform float _laserEnabled1;
uniform float _dotScale1;
uniform float _roll1;
uniform float _texelAreaCM1;
uniform vec3 _surfaceColor1;
uniform float _texSize1;
uniform float _gloss1;
uniform float _refl1;
uniform float _normalBias1;
uniform float _max_reflection1;
float _a0061;
float _a0063;
vec2 _v0065;
float _a0069;
vec2 _v0075;
float _a0079;
vec2 _c0085;
float _x0089;
float _TMP90;
float _x0099;
float _TMP100;
float _x0109;
float _TMP110;
vec2 _c0117;
vec2 _c0119;
vec2 _c0121;
vec2 _c0123;
vec3 _v0125;
vec2 _c0145;
vec3 _v0155;
vec4 _TMP162;
vec4 _tmp0163;
vec2 _c0173;
float _TMP174;

 // main procedure, the original name was main
void main()
{

    pixel_out _OUT;
    float _offset;
    vec2 _intPt2;
    float _oz1;
    float _texelAreaIN21;
    float _texelVolIN31;
    float _texelThicknessIN1;
    float _mils1;
    float _a1;
    float _b1;
    float _dx1;
    float _dy1;
    vec3 _N1;
    float _ndotl1;
    float _ndoth1;
    float _reflection_factor1;
    vec3 _diffuse1;

    _offset = (_distance1 - _focaldistance1)/1.00000000E+003;
    _a0061 = -_roll1;
    _TMP21 = sin(float(_a0061));
    _TMP0 = float(_TMP21);
    _a0063 = -_roll1;
    _TMP22 = cos(float(_a0063));
    _TMP1 = float(_TMP22);
    _intPt2 = vec2(_intPt1.x + _TMP0*_offset, _intPt1.y + _TMP1*_offset);
    _v0065 = _texcoord.xy - _intPt1;
    _TMP30 = dot(vec2(float(_v0065.x), float(_v0065.y)), vec2(float(_v0065.x), float(_v0065.y)));
    _TMP23 = float(_TMP30);
    _a0069 = float(_TMP23);
    _TMP31 = inversesqrt(_a0069);
    _TMP24 = 1.00000000E+000/_TMP31;
    _TMP2 = float(_TMP24);
    _TMP25 = abs(float(_TMP2));
    _TMP3 = float(_TMP25);
    _v0075 = _texcoord.xy - _intPt2;
    _TMP30 = dot(vec2(float(_v0075.x), float(_v0075.y)), vec2(float(_v0075.x), float(_v0075.y)));
    _TMP23 = float(_TMP30);
    _a0079 = float(_TMP23);
    _TMP31 = inversesqrt(_a0079);
    _TMP24 = 1.00000000E+000/_TMP31;
    _TMP4 = float(_TMP24);
    _TMP25 = abs(float(_TMP4));
    _TMP5 = float(_TMP25);
    if (_laserEnabled1 == 1.00000000E+000 && (_TMP3 < _dotScale1 || _TMP5 < _dotScale1)) { // if begin
        _OUT._color = vec3( 1.00000000E+000, 0.00000000E+000, 0.00000000E+000);
    } else {
        if (_passthrough1 == 1.00000000E+000) { // if begin
            _OUT._color = vec3( 1.00000000E+000, 1.00000000E+000, 1.00000000E+000);
        } else {
            if (_AccumEnabled1 == 1.00000000E+000) { // if begin
                _c0085 = vec2(float((_texcoord.xy*_texSize1).x), float((_texcoord.xy*_texSize1).y));
                _TMP6 = texture2DRect(_texture1, _c0085);
                _oz1 = float(_TMP6.w);
                _texelAreaIN21 = _texelAreaCM1*1.55000001E-001;
                _texelVolIN31 = _oz1*1.80467999E+000;
                _texelThicknessIN1 = _texelVolIN31/_texelAreaIN21;
                _mils1 = float((_texelThicknessIN1*1.00000000E+003));
                _a1 = float(_targetThickness1);
                _b1 = float(_targetThicknessMax1);
                if (_oz1 <= 0.00000000E+000) { // if begin
                    _OUT._color = _surfaceColor1;
                } else {
                    if (_mils1 < _a1) { // if begin
                        _x0089 = _mils1/_a1;
                        _TMP29 = min(1.00000000E+000, _x0089);
                        _TMP90 = max(0.00000000E+000, _TMP29);
                        _TMP7 = _TMP90*_TMP90*(3.00000000E+000 - 2.00000000E+000*_TMP90);
                        _OUT._color = vec3(0.00000000E+000, 0.00000000E+000, float((5.00000000E-001*_TMP7 + 5.00000000E-001)));
                    } else {
                        if (_mils1 < _b1) { // if begin
                            _x0099 = (_mils1 - _a1)/(_b1 - _a1);
                            _TMP29 = min(1.00000000E+000, _x0099);
                            _TMP100 = max(0.00000000E+000, _TMP29);
                            _TMP8 = _TMP100*_TMP100*(3.00000000E+000 - 2.00000000E+000*_TMP100);
                            _OUT._color = vec3(0.00000000E+000, float((_TMP8*5.00000000E-001 + 5.00000000E-001)), 0.00000000E+000);
                        } else {
                            if (_mils1 >= _b1) { // if begin
                                _x0109 = (_mils1 - _b1)/(2.00000000E+001 - _b1);
                                _TMP29 = min(1.00000000E+000, _x0109);
                                _TMP110 = max(0.00000000E+000, _TMP29);
                                _TMP9 = _TMP110*_TMP110*(3.00000000E+000 - 2.00000000E+000*_TMP110);
                                _OUT._color = vec3(float((vec3(float(float((5.00000000E-001*(1.00000000E+000 - _TMP9) + 5.00000000E-001))), 0.00000000E+000, 0.00000000E+000) - 2.50000000E-001).x), float((vec3(float(float((5.00000000E-001*(1.00000000E+000 - _TMP9) + 5.00000000E-001))), 0.00000000E+000, 0.00000000E+000) - 2.50000000E-001).y), float((vec3(float(float((5.00000000E-001*(1.00000000E+000 - _TMP9) + 5.00000000E-001))), 0.00000000E+000, 0.00000000E+000) - 2.50000000E-001).z));
                            } else {
                                _OUT._color = vec3( 1.00000000E+000, 1.00000000E+000, 1.00000000E+000);
                            } // end if
                        } // end if
                    } // end if
                } // end if
            } else {
                _c0117 = vec2(float((_texcoord.x*_texSize1 - 1.00000000E+000)), float((_texcoord.y*_texSize1)));
                _TMP10 = texture2DRect(_texture1, _c0117);
                _c0119 = vec2(float((_texcoord.x*_texSize1 + 1.00000000E+000)), float((_texcoord.y*_texSize1)));
                _TMP11 = texture2DRect(_texture1, _c0119);
                _dx1 = _TMP10.w - _TMP11.w;
                _c0121 = vec2(float((_texcoord.x*_texSize1)), float((_texcoord.y*_texSize1 - 1.00000000E+000)));
                _TMP12 = texture2DRect(_texture1, _c0121);
                _c0123 = vec2(float((_texcoord.x*_texSize1)), float((_texcoord.y*_texSize1 + 1.00000000E+000)));
                _TMP13 = texture2DRect(_texture1, _c0123);
                _dy1 = _TMP12.w - _TMP13.w;
                _v0125 = _N.xyz + (1.00000000E+004*_normalBias1)*vec3(_dx1, _dy1, 0.00000000E+000);
                _TMP26 = dot(_v0125, _v0125);
                _TMP27 = inversesqrt(_TMP26);
                _N1 = _TMP27*_v0125;
                _c0145 = vec2(float((_texcoord.xy*_texSize1).x), float((_texcoord.xy*_texSize1).y));
                _TMP15 = texture2DRect(_texture1, _c0145);
                _TMP26 = dot(_L.xyz, _L.xyz);
                _TMP27 = inversesqrt(_TMP26);
                _TMP16 = _TMP27*_L.xyz;
                _ndotl1 = dot(_N1, _TMP16);
                _v0155 = _L.xyz + _V.xyz;
                _TMP26 = dot(_v0155, _v0155);
                _TMP27 = inversesqrt(_TMP26);
                _TMP17 = _TMP27*_v0155;
                _ndoth1 = dot(_N1, _TMP17);
                _tmp0163 = vec4(_ndotl1, _ndoth1, _gloss1, _gloss1);
                if (_tmp0163.x > 0.00000000E+000) { // if begin
                    _TMP33 = max(0.00000000E+000, _tmp0163.y);
                    _TMP32 = pow(_TMP33, _tmp0163.z);
                } else {
                    _TMP32 = 0.00000000E+000;
                } // end if
                _TMP34 = max(0.00000000E+000, _tmp0163.x);
                _TMP162 = vec4(1.00000000E+000, _TMP34, _TMP32, 1.00000000E+000);
                _c0173 = vec2(float((_texcoord.xy*_texSize1).x), float((_texcoord.xy*_texSize1).y));
                _TMP19 = texture2DRect(_texture1, _c0173);
                _reflection_factor1 = _TMP162.z*_TMP19.w*1.00000000E+006*_refl1;
                _TMP29 = min(_max_reflection1, _reflection_factor1);
                _TMP174 = max(0.00000000E+000, _TMP29);
                _diffuse1 = _TMP162.y*_TMP15.xyz;
                _OUT._color = vec3(float((_diffuse1 + vec3(_TMP174, _TMP174, _TMP174)).x), float((_diffuse1 + vec3(_TMP174, _TMP174, _TMP174)).y), float((_diffuse1 + vec3(_TMP174, _TMP174, _TMP174)).z));
            } // end if
        } // end if
    } // end if
    gl_FragColor.xyz = vec4(float(_OUT._color.x), float(_OUT._color.y), float(_OUT._color.z), 1.0);
    return;
} // main end
