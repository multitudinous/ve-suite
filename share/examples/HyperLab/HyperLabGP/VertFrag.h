#ifndef VERT_FRAG_H
#define VERT_FRAG_H

//////////////////////////////////////////////////////////////////
//                        Base Shader                           //
//////////////////////////////////////////////////////////////////

char base_vertex[]=
    "varying vec3 color; \n"

    "void main() \n"
    "{ \n"
        "gl_Position = ftransform(); \n"

        "vec3 eyePos = vec3( gl_ModelViewMatrix * gl_Vertex ); \n"
        "vec3 lightPos = gl_LightSource[ 1 ].position.xyz; \n"
        "vec3 normal = vec3( gl_NormalMatrix * gl_Normal ); \n"

        "vec3 N = normalize( normal ); \n"
        "vec3 L = normalize( lightPos ); \n"
        "float NDotL = max( 0.0, dot( N, L ) ); \n"

        "vec3 TotalAmbient = gl_LightSource[ 1 ].ambient.rgb * \n"
                            "gl_FrontMaterial.ambient.rgb; \n"
        "vec3 TotalDiffuse = gl_LightSource[ 1 ].diffuse.rgb * \n"
                            "gl_FrontMaterial.diffuse.rgb*NDotL; \n"
        "vec3 TotalSpecular = gl_LightSource[ 1 ].specular.rgb * \n"
                             "gl_FrontMaterial.specular.rgb * \n"
                             "pow( NDotL, gl_FrontMaterial.shininess ); \n"

        "color = TotalAmbient + TotalDiffuse + TotalSpecular; \n"
        "color *= gl_FrontMaterial.emission.rgb; \n"
    "} \n";

char base_fragment[]=
    "varying vec3 color; \n"

    "void main() \n"
    "{ \n"
        "gl_FragColor = vec4( color, 1.0 ); \n"
    "} \n";

//////////////////////////////////////////////////////////////////
//                        Phong Shader                          //
//////////////////////////////////////////////////////////////////

char phong_vertex[]=
   "varying vec3 eyePos; \n"
   "varying vec3 lightPos; \n"
   "varying vec3 normal; \n"

   "void main() \n"
   "{ \n"
         "gl_Position=ftransform(); \n"
   
         "eyePos=vec3(gl_ModelViewMatrix*gl_Vertex); \n"
         "lightPos=gl_LightSource[1].position.xyz; \n"
         "normal=vec3(gl_NormalMatrix*gl_Normal); \n"
   "} \n";

char phong_fragment[]=
   "varying vec3 eyePos; \n"
   "varying vec3 lightPos; \n"
   "varying vec3 normal; \n"

   "void main() \n"
   "{ \n"   
         "vec3 N=normalize(normal); \n"
         "vec3 L=normalize(lightPos); \n"
         "float NDotL=max(dot(N,L),0.0); \n"
   
         "vec3 V=normalize(eyePos); \n"
         "vec3 R=reflect(V,N); \n"
         "float RDotL=max(dot(R,L),0.0); \n"

         "vec3 TotalAmbient=gl_LightSource[1].ambient.rgb*gl_FrontMaterial.ambient.rgb; \n"
         "vec3 TotalDiffuse=gl_LightSource[1].diffuse.rgb*gl_FrontMaterial.diffuse.rgb*NDotL; \n"
         "vec3 TotalSpecular=gl_LightSource[1].specular.rgb*gl_FrontMaterial.specular.rgb*pow(RDotL,gl_FrontMaterial.shininess); \n"
  
         "vec3 color=TotalAmbient+TotalDiffuse+TotalSpecular; \n"
         "color*=gl_FrontMaterial.emission.rgb; \n"

         "gl_FragColor=vec4(color,1.0); \n"
   "} \n";

//////////////////////////////////////////////////////////////////
//                       Texture Shader                         //
//////////////////////////////////////////////////////////////////

char texture_vertex[]=
   "varying vec3 TotalAmbient; \n"
   "varying vec3 TotalDiffuse; \n"
   "varying vec3 TotalSpecular; \n"

   "varying vec2 texCoord; \n"

   "void main() \n"
   "{ \n"
         "gl_Position=ftransform(); \n"

         "texCoord=gl_MultiTexCoord0.xy; \n"
         "vec3 eyePos=vec3(gl_ModelViewMatrix*gl_Vertex); \n"
         "vec3 lightPos=gl_LightSource[1].position.xyz; \n"
         "vec3 normal=vec3(gl_NormalMatrix*gl_Normal); \n"

         "vec3 N=normalize(normal); \n"
         "vec3 L=normalize(lightPos); \n"
         "float NDotL=max(0.0,dot(N,L)); \n"
   
         //"vec3 V=normalize(eyePos); \n"
         //"vec3 R=reflect(V,N); \n"
         //"float RDotL=max(0.0,dot(R,L)); \n"   
 
         "TotalAmbient=gl_LightSource[1].ambient.rgb*gl_FrontMaterial.ambient.rgb; \n"
         "TotalDiffuse=gl_LightSource[1].diffuse.rgb*gl_FrontMaterial.diffuse.rgb*NDotL; \n"
         "TotalSpecular=gl_LightSource[1].specular.rgb*gl_FrontMaterial.specular.rgb*pow(NDotL,gl_FrontMaterial.shininess); \n"
   "} \n";

char texture_fragment[]=
   "uniform sampler2D baseMap; \n"

   "varying vec3 TotalAmbient; \n"
   "varying vec3 TotalDiffuse; \n"
   "varying vec3 TotalSpecular; \n"

   "varying vec2 texCoord; \n"

   "void main() \n"
   "{ \n"
         "vec3 baseColor=vec3(texture2D(baseMap,texCoord)); \n"

         "TotalAmbient*=baseColor; \n"
         "TotalDiffuse*=baseColor; \n"

         "vec3 color=TotalAmbient+TotalDiffuse+TotalSpecular; \n"
         "color*=gl_FrontMaterial.emission.rgb; \n"

         "gl_FragColor=vec4(color,1.0); \n"
   "} \n";

//////////////////////////////////////////////////////////////////
//                         PCF Shader                           //
//////////////////////////////////////////////////////////////////

char pcf_vertex[]=
	"varying vec3 TotalAmbient; \n"
   "varying vec3 TotalDiffuse; \n"
   "varying vec3 TotalSpecular; \n"

   "void main() \n"
   "{ \n"
	      "gl_Position=ftransform(); \n"

         "vec4 eyePos=gl_ModelViewMatrix*gl_Vertex; \n"
         "vec3 lightPos=gl_LightSource[1].position.xyz; \n"
         "vec3 normal=vec3(gl_NormalMatrix*gl_Normal); \n"

         "vec3 N=normalize(normal); \n"
         "vec3 L=normalize(lightPos); \n"
         "float NDotL=max(0.0,dot(N,L)); \n"
   
         //"vec3 V=normalize(eyePos.xyz); \n"
         //"vec3 R=reflect(V,N); \n"
         //"float RDotL=max(0.0,dot(R,L)); \n"   
 
         "TotalAmbient=gl_LightSource[1].ambient.rgb*gl_FrontMaterial.ambient.rgb; \n"
         "TotalDiffuse=gl_LightSource[1].diffuse.rgb*gl_FrontMaterial.diffuse.rgb*NDotL; \n"
         "TotalSpecular=gl_LightSource[1].specular.rgb*gl_FrontMaterial.specular.rgb*pow(NDotL,gl_FrontMaterial.shininess); \n"
  
         "gl_TexCoord[0].s=dot(eyePos,gl_EyePlaneS[0]); \n"
         "gl_TexCoord[0].t=dot(eyePos,gl_EyePlaneT[0]); \n"
         "gl_TexCoord[0].p=dot(eyePos,gl_EyePlaneR[0]); \n"
         "gl_TexCoord[0].q=dot(eyePos,gl_EyePlaneQ[0]); \n"
   "} \n";

char pcf_fragment[]=
   "uniform sampler2DShadow shadowMap; \n"

	"varying vec3 TotalAmbient; \n"
   "varying vec3 TotalDiffuse; \n"
   "varying vec3 TotalSpecular; \n"

   "void main() \n"
   "{ \n"
         "vec4 color=vec4(TotalAmbient+TotalDiffuse+TotalSpecular,1.0); \n"
         "color*=gl_FrontMaterial.emission; \n"

	      "const float kTransparency=0.4; \n"

         "vec3 shadowUV=gl_TexCoord[0].xyz/gl_TexCoord[0].q; \n"
	      "float mapScale=1/4096.0; \n"

         "vec4 shadowColor=shadow2D(shadowMap,shadowUV); \n"

         "for(int i=1;i<11;i++){ \n"
	            //"shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(i*mapScale,i*mapScale,0)); \n"
	            //"shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(i*mapScale,-i*mapScale,0)); \n"
	            //"shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(-i*mapScale,i*mapScale,0)); \n"
	            //"shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(-i*mapScale,-i*mapScale,0)); \n"
               "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(i*mapScale,0,0)); \n"
	            "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(-i*mapScale,0,0)); \n"
	            "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(0,i*mapScale,0)); \n"
	            "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(0,-i*mapScale,0)); \n"
         "} \n"

	      "shadowColor=shadowColor/71.0; \n"

	      "shadowColor+=kTransparency; \n"
	      "shadowColor=clamp(shadowColor,0.0,1.0); \n"

	      "if(shadowUV.x>=0.0&&shadowUV.y>=0.0&&shadowUV.x<=1.0&&shadowUV.y<=1.0) \n"
	      "{ \n"
               "gl_FragColor=color*shadowColor; \n"
	      "} \n"

	      "else \n"
	      "{ \n"
		         "gl_FragColor=color; \n"
	      "} \n"
   "} \n";

//////////////////////////////////////////////////////////////////
//                      Reflection Shader                       //
//////////////////////////////////////////////////////////////////

char reflection_vertex[]=
	"varying vec3 TotalAmbient; \n"
   "varying vec3 TotalDiffuse; \n"
   "varying vec3 TotalSpecular; \n"

   "varying vec3 R; \n"

   "void main() \n"
   "{ \n"
         "gl_Position=ftransform(); \n"

         "vec3 eyePos=vec3(gl_ModelViewMatrix*gl_Vertex); \n"
         "vec3 lightPos=gl_LightSource[1].position.xyz; \n"
         "vec3 normal=vec3(gl_NormalMatrix*gl_Normal); \n"
   
         "vec3 N=normalize(normal); \n"
         "vec3 L=normalize(lightPos); \n"
         "float NDotL=max(0.0,dot(N,L)); \n"
   
         "vec3 V=normalize(eyePos); \n"
         "R=reflect(V,N); \n"
         "float RDotL=max(0.0,dot(R,L)); \n"   
 
         "TotalAmbient=gl_LightSource[1].ambient.rgb*gl_FrontMaterial.ambient.rgb; \n"
         "TotalDiffuse=gl_LightSource[1].diffuse.rgb*gl_FrontMaterial.diffuse.rgb*NDotL; \n"
         "TotalSpecular=gl_LightSource[1].specular.rgb*gl_FrontMaterial.specular.rgb*pow(NDotL,gl_FrontMaterial.shininess); \n"
   "} \n";

char reflection_fragment[]=
   "uniform samplerCube envMap; \n"

   "varying vec3 TotalAmbient; \n"
   "varying vec3 TotalDiffuse; \n"
   "varying vec3 TotalSpecular; \n"

   "varying vec3 R; \n"

   "void main() \n"
   "{ \n"
         "vec3 color=TotalAmbient+TotalDiffuse+TotalSpecular; \n"
         "color*=gl_FrontMaterial.emission.rgb; \n"

         "vec3 reflection_color=textureCube(envMap,R).rgb; \n"

         "color=mix(color,reflection_color,0.05); \n"

         "gl_FragColor=vec4(color,1.0); \n"
   "} \n";

//////////////////////////////////////////////////////////////////
//                         XRay Shader                          //
//////////////////////////////////////////////////////////////////

char xray_vertex[]=
   "varying vec3 eyePos; \n"
   "varying vec3 lightPos; \n"
   "varying vec3 normal; \n"

   "void main() \n"
   "{ \n"
         "gl_Position=ftransform(); \n"

         "eyePos=vec3(gl_ModelViewMatrix*gl_Vertex); \n"
         "lightPos=gl_LightSource[1].position.xyz; \n"
         "normal=vec3(gl_NormalMatrix*gl_Normal); \n"
   "} \n";

char xray_fragment[]=
   "varying vec3 eyePos; \n"
   "varying vec3 lightPos; \n"
   "varying vec3 normal; \n"

   "void main() \n"
   "{ \n"
         "vec3 N=normalize(normal); \n"
         "vec3 L=normalize(lightPos); \n"
         "float NDotL=max(dot(N,L),0.0); \n" 
   
         "vec3 V=normalize(eyePos); \n"
         "vec3 R=reflect(V,N); \n"
         "float RDotL=max(dot(R,L),0.0); \n"

         "float opac=dot(N,V); \n"
         "opac=abs(opac); \n"
         "opac=1.0-pow(opac,3.0); \n"

         "vec3 TotalAmbient=gl_LightSource[1].ambient.rgb*gl_FrontMaterial.ambient.rgb; \n"
         "vec3 TotalDiffuse=gl_LightSource[1].diffuse.rgb*gl_FrontMaterial.diffuse.rgb*NDotL; \n"
         "vec3 TotalSpecular=gl_LightSource[1].specular.rgb*gl_FrontMaterial.specular.rgb*pow(RDotL,gl_FrontMaterial.shininess); \n"
         
         "vec3 color=TotalAmbient+TotalDiffuse+TotalSpecular; \n"
         "color*=gl_FrontMaterial.emission.rgb; \n"

         "gl_FragColor=opac*vec4(color,1.0); \n"
   "} \n";

//////////////////////////////////////////////////////////////////
//                    Phong_Texture Shader                      //
//////////////////////////////////////////////////////////////////

char phong_texture_vertex[]=
   "varying vec2 texCoord; \n"
   "varying vec3 eyePos; \n"
   "varying vec3 lightPos; \n"
   "varying vec3 normal; \n"

   "void main() \n"
   "{ \n"
         "gl_Position=ftransform(); \n"

         "texCoord=gl_MultiTexCoord0.xy; \n"
         "eyePos=vec3(gl_ModelViewMatrix*gl_Vertex); \n"
         "lightPos=gl_LightSource[1].position.xyz; \n"
         "normal=vec3(gl_NormalMatrix*gl_Normal); \n"
   "} \n";

char phong_texture_fragment[]=
   "uniform sampler2D baseMap; \n"

   "varying vec2 texCoord; \n"
   "varying vec3 eyePos; \n"
   "varying vec3 lightPos; \n"
   "varying vec3 normal; \n"

   "void main() \n"
   "{ \n"   
         "vec3 N=normalize(normal); \n"
         "vec3 L=normalize(lightPos); \n"
         "float NDotL=max(dot(N,L),0.0); \n" 
   
         "vec3 V=normalize(eyePos); \n"
         "vec3 R=reflect(V,N); \n"
         "float RDotL=max(dot(R,L),0.0); \n"

         "vec3 baseColor=vec3(texture2D(baseMap,texCoord)); \n"

         "vec3 TotalAmbient=gl_LightSource[1].ambient.rgb*gl_FrontMaterial.ambient.rgb*baseColor; \n"
         "vec3 TotalDiffuse=gl_LightSource[1].diffuse.rgb*gl_FrontMaterial.diffuse.rgb*baseColor*NDotL; \n"
         "vec3 TotalSpecular=gl_LightSource[1].specular.rgb*gl_FrontMaterial.specular.rgb*pow(RDotL,gl_FrontMaterial.shininess); \n"

         "vec3 color=TotalAmbient+TotalDiffuse+TotalSpecular; \n"
         "color*=gl_FrontMaterial.emission.rgb; \n"

         "gl_FragColor=vec4(color,1.0); \n"
   "} \n";

//////////////////////////////////////////////////////////////////
//                       Phong_PCF Shader                       //
//////////////////////////////////////////////////////////////////

char phong_pcf_vertex[]=
	"varying vec3 lightPos; \n"
   "varying vec3 normal; \n"
   "varying vec4 eyePos; \n"

   "void main() \n"
   "{ \n"
	      "gl_Position=ftransform(); \n"

         "eyePos=gl_ModelViewMatrix*gl_Vertex; \n"
         "lightPos=gl_LightSource[1].position.xyz; \n"
         "normal=vec3(gl_NormalMatrix*gl_Normal); \n"

         "gl_TexCoord[0].s=dot(eyePos,gl_EyePlaneS[0]); \n"
         "gl_TexCoord[0].t=dot(eyePos,gl_EyePlaneT[0]); \n"
         "gl_TexCoord[0].p=dot(eyePos,gl_EyePlaneR[0]); \n"
         "gl_TexCoord[0].q=dot(eyePos,gl_EyePlaneQ[0]); \n"
   "} \n";

char phong_pcf_fragment[]=
   "uniform sampler2DShadow shadowMap; \n"

	"varying vec3 lightPos; \n"
   "varying vec3 normal; \n"
   "varying vec4 eyePos; \n"

   "void main() \n"
   "{ \n"
         "vec3 N=normalize(normal); \n"
         "vec3 L=normalize(lightPos); \n"
         "float NDotL=max(dot(N,L),0.0); \n"

         "vec3 V=normalize(eyePos.xyz); \n"
         "vec3 R=reflect(V,N); \n"
         "float RDotL=max(dot(R,L),0.0); \n"

         "vec3 TotalAmbient=gl_LightSource[1].ambient.rgb*gl_FrontMaterial.ambient.rgb; \n"
         "vec3 TotalDiffuse=gl_LightSource[1].diffuse.rgb*gl_FrontMaterial.diffuse.rgb*NDotL; \n"
         "vec3 TotalSpecular=gl_LightSource[1].specular.rgb*gl_FrontMaterial.specular.rgb*pow(RDotL,gl_FrontMaterial.shininess); \n"

         "vec4 color=vec4(TotalAmbient+TotalDiffuse+TotalSpecular,1.0); \n"
         "color*=gl_FrontMaterial.emission; \n"

	      "const float kTransparency=0.4; \n"     

         "vec3 shadowUV=gl_TexCoord[0].xyz/gl_TexCoord[0].q; \n"
	      "float mapScale=1/4096.0; \n"

         "vec4 shadowColor=shadow2D(shadowMap,shadowUV); \n"

         "for(int i=1;i<11;i++){ \n"
	         //"shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(i*mapScale,i*mapScale,0)); \n"
	         //"shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(i*mapScale,-i*mapScale,0)); \n"
	         //"shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(-i*mapScale,i*mapScale,0)); \n"
	         //"shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(-i*mapScale,-i*mapScale,0)); \n"
            "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(i*mapScale,0,0)); \n"
	         "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(-i*mapScale,0,0)); \n"
	         "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(0,i*mapScale,0)); \n"
	         "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(0,-i*mapScale,0)); \n"
         "} \n"

	      "shadowColor=shadowColor/71.0; \n"

	      "shadowColor+=kTransparency; \n"
	      "shadowColor=clamp(shadowColor,0.0,1.0); \n"

	      "if(shadowUV.x>=0.0&&shadowUV.y>=0.0&&shadowUV.x<=1.0&&shadowUV.y<=1.0) \n"
	      "{ \n"
		      "gl_FragColor=color*shadowColor; \n"
	      "} \n"

	      "else \n"
	      "{ \n"
		      "gl_FragColor=color; \n"
	      "} \n"
   "} \n";

//////////////////////////////////////////////////////////////////
//                   Phong_Reflection Shader                    //
//////////////////////////////////////////////////////////////////

char phong_reflection_vertex[]=
	"varying vec3 lightPos; \n"
   "varying vec3 normal; \n"
   "varying vec3 eyePos; \n"

   "void main() \n"
   "{ \n"
	      "gl_Position=ftransform(); \n"

         "eyePos=vec3(gl_ModelViewMatrix*gl_Vertex); \n"
         "lightPos=gl_LightSource[1].position.xyz; \n"
         "normal=vec3(gl_NormalMatrix*gl_Normal); \n"
   "} \n";

char phong_reflection_fragment[]=
   "uniform samplerCube envMap; \n"

	"varying vec3 lightPos; \n"
   "varying vec3 normal; \n"
   "varying vec3 eyePos; \n"

   "void main() \n"
   "{ \n"

         "vec3 N=normalize(normal); \n"
         "vec3 L=normalize(lightPos); \n"
         "float NDotL=max(dot(N,L),0.0); \n"

         "vec3 V=normalize(eyePos.xyz); \n"
         "vec3 R=reflect(V,N); \n"
         "float RDotL=max(dot(R,L),0.0); \n"

         "vec3 TotalAmbient=gl_LightSource[1].ambient.rgb*gl_FrontMaterial.ambient.rgb; \n"
         "vec3 TotalDiffuse=gl_LightSource[1].diffuse.rgb*gl_FrontMaterial.diffuse.rgb*NDotL; \n"
         "vec3 TotalSpecular=gl_LightSource[1].specular.rgb*gl_FrontMaterial.specular.rgb*pow(NDotL,gl_FrontMaterial.shininess); \n"

         "vec3 color=TotalAmbient+TotalDiffuse+TotalSpecular; \n"
         "color*=gl_FrontMaterial.emission; \n"

         "vec3 reflection_color=textureCube(envMap,R).rgb; \n"

         "gl_FragColor=vec4(mix(color,reflection_color,0.05),1.0); \n"
   "} \n";

//////////////////////////////////////////////////////////////////
//                      Texture_PCF Shader                      //
//////////////////////////////////////////////////////////////////

char texture_pcf_vertex[]=
   "varying vec3 TotalAmbient; \n"
   "varying vec3 TotalDiffuse; \n"
   "varying vec3 TotalSpecular; \n"

   "varying vec2 texCoord; \n"

   "void main() \n"
   "{ \n"
	      "gl_Position=ftransform(); \n"

         "texCoord=gl_MultiTexCoord0.xy; \n"
         "vec4 eyePos=gl_ModelViewMatrix*gl_Vertex; \n"
         "vec3 lightPos=gl_LightSource[1].position.xyz; \n"
         "vec3 normal=vec3(gl_NormalMatrix*gl_Normal); \n"

         "vec3 N=normalize(normal); \n"
         "vec3 L=normalize(lightPos); \n"
         "float NDotL=max(0.0,dot(N,L)); \n"
   
         //"vec3 V=normalize(eyePos.xyz); \n"
         //"vec3 R=reflect(V,N); \n"
         //"float RDotL=max(0.0,dot(R,L)); \n"   
 
         "TotalAmbient=gl_LightSource[1].ambient.rgb*gl_FrontMaterial.ambient.rgb; \n"
         "TotalDiffuse=gl_LightSource[1].diffuse.rgb*gl_FrontMaterial.diffuse.rgb*NDotL; \n"
         "TotalSpecular=gl_LightSource[1].specular.rgb*gl_FrontMaterial.specular.rgb*pow(NDotL,gl_FrontMaterial.shininess); \n"

         "gl_TexCoord[0].s=dot(eyePos,gl_EyePlaneS[0]); \n"
         "gl_TexCoord[0].t=dot(eyePos,gl_EyePlaneT[0]); \n"
         "gl_TexCoord[0].p=dot(eyePos,gl_EyePlaneR[0]); \n"
         "gl_TexCoord[0].q=dot(eyePos,gl_EyePlaneQ[0]); \n"
   "} \n";

char texture_pcf_fragment[]=
   "uniform sampler2DShadow shadowMap; \n"

	"varying vec3 TotalAmbient; \n"
   "varying vec3 TotalDiffuse; \n"
   "varying vec3 TotalSpecular; \n"

   "varying vec2 texCoord; \n"

   "void main() \n"
   "{ \n"
         "vec3 baseColor=vec3(texture2D(baseMap,texCoord)); \n"

         "TotalAmbient*=baseColor; \n"
         "TotalDiffuse*=baseColor; \n"

         "vec4 color=vec4(TotalAmbient+TotalDiffuse+TotalSpecular,1.0); \n"
         "color*=gl_FrontMaterial.emission; \n"

	      "const float kTransparency=0.4; \n"

         "vec3 shadowUV=gl_TexCoord[0].xyz/gl_TexCoord[0].q; \n"
	      "float mapScale=1.0/4096.0; \n"

         "vec4 shadowColor=shadow2D(shadowMap,shadowUV); \n"

         "for(int i=1;i<11;i++){ \n"
	            "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(i*mapScale,i*mapScale,0)); \n"
	            "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(i*mapScale,-i*mapScale,0)); \n"
	            "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(-i*mapScale,i*mapScale,0)); \n"
	            "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(-i*mapScale,-i*mapScale,0)); \n"
               "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(i*mapScale,0,0)); \n"
	            "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(-i*mapScale,0,0)); \n"
	            "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(0,i*mapScale,0)); \n"
	            "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(0,-i*mapScale,0)); \n"
         "} \n"

	      "shadowColor=shadowColor/71.0; \n"

	      "shadowColor+=kTransparency; \n"
	      "shadowColor=clamp(shadowColor,0.0,1.0); \n"

	      "if(shadowUV.x>=0.0&&shadowUV.y>=0.0&&shadowUV.x<=1.0&&shadowUV.y<=1.0) \n"
	      "{ \n"
		         "gl_FragColor=color*shadowColor; \n"
	      "} \n"

	      "else \n"
	      "{ \n"
		         "gl_FragColor=color; \n"
	      "} \n"
   "} \n";

//////////////////////////////////////////////////////////////////
//                  Texture_Reflection Shader                   //
//////////////////////////////////////////////////////////////////

char texture_reflection_vertex[]=
   "varying vec3 TotalAmbient; \n"
   "varying vec3 TotalDiffuse; \n"
   "varying vec3 TotalSpecular; \n"

   "varying vec3 R; \n"

   "varying vec2 texCoord; \n"

   "void main() \n"
   "{ \n"
         "gl_Position=ftransform(); \n"

         "texCoord=gl_MultiTexCoord0.xy; \n"
         "vec3 eyePos=vec3(gl_ModelViewMatrix*gl_Vertex); \n"
         "vec3 lightPos=gl_LightSource[1].position.xyz; \n"
         "vec3 normal=vec3(gl_NormalMatrix*gl_Normal); \n"

         "vec3 N=normalize(normal); \n"
         "vec3 L=normalize(lightPos); \n"
         "float NDotL=max(0.0,dot(N,L)); \n"
   
         "vec3 V=normalize(eyePos); \n"
         "R=reflect(V,N); \n"
         //"float RDotL=max(0.0,dot(R,L)); \n"   
 
         "TotalAmbient=gl_LightSource[1].ambient.rgb*gl_FrontMaterial.ambient.rgb; \n"
         "TotalDiffuse=gl_LightSource[1].diffuse.rgb*gl_FrontMaterial.diffuse.rgb*NDotL; \n"
         "TotalSpecular=gl_LightSource[1].specular.rgb*gl_FrontMaterial.specular.rgb*pow(NDotL,gl_FrontMaterial.shininess); \n"
   "} \n";

char texture_reflection_fragment[]=
   "uniform samplerCube envMap; \n"
   "uniform sampler2D baseMap; \n"

   "varying vec3 TotalAmbient; \n"
   "varying vec3 TotalDiffuse; \n"
   "varying vec3 TotalSpecular; \n"

   "varying vec3 R; \n"

   "varying vec2 texCoord; \n"

   "void main() \n"
   "{ \n"
         "vec3 baseColor=vec3(texture2D(baseMap,texCoord)); \n"

         "TotalAmbient*=baseColor; \n"
         "TotalDiffuse*=baseColor; \n"

         "vec3 color=TotalAmbient+TotalDiffuse+TotalSpecular; \n"
         "color*=gl_FrontMaterial.emission.rgb; \n"

         "vec3 reflection_color=textureCube(envMap,R).rgb; \n"
         "color=mix(color,reflection_color,0.05); \n"

         "gl_FragColor=vec4(color,1.0); \n"
   "} \n";

//////////////////////////////////////////////////////////////////
//                    PCF_Reflection Shader                     //
//////////////////////////////////////////////////////////////////

char pcf_reflection_vertex[]=
   "varying vec3 TotalAmbient; \n"
   "varying vec3 TotalDiffuse; \n"
   "varying vec3 TotalSpecular; \n"

   "varying vec3 R; \n"

   "void main() \n"
   "{ \n"
	      "gl_Position=ftransform(); \n"

         "vec4 eyePos=gl_ModelViewMatrix*gl_Vertex; \n"
         "vec3 lightPos=gl_LightSource[1].position.xyz; \n"
         "vec3 normal=vec3(gl_NormalMatrix*gl_Normal); \n"

         "vec3 N=normalize(normal); \n"
         "vec3 L=normalize(lightPos); \n"
         "float NDotL=max(0.0,dot(N,L)); \n"
   
         "vec3 V=normalize(eyePos.xyz); \n"
         "R=reflect(V,N); \n"
         //"float RDotL=max(0.0,dot(R,L)); \n"   
 
         "TotalAmbient=gl_LightSource[1].ambient.rgb*gl_FrontMaterial.ambient.rgb; \n"
         "TotalDiffuse=gl_LightSource[1].diffuse.rgb*gl_FrontMaterial.diffuse.rgb*NDotL; \n"
         "TotalSpecular=gl_LightSource[1].specular.rgb*gl_FrontMaterial.specular.rgb*pow(NDotL,gl_FrontMaterial.shininess); \n"

         "gl_TexCoord[0].s=dot(eyePos,gl_EyePlaneS[0]); \n"
         "gl_TexCoord[0].t=dot(eyePos,gl_EyePlaneT[0]); \n"
         "gl_TexCoord[0].p=dot(eyePos,gl_EyePlaneR[0]); \n"
         "gl_TexCoord[0].q=dot(eyePos,gl_EyePlaneQ[0]); \n"
   "} \n";

char pcf_reflection_fragment[]=
   "uniform samplerCube envMap; \n"
   "uniform sampler2DShadow shadowMap; \n"

	"varying vec3 TotalAmbient; \n"
   "varying vec3 TotalDiffuse; \n"
   "varying vec3 TotalSpecular; \n"

   "varying vec3 R; \n"

   "void main() \n"
   "{ \n"
         "vec4 color=vec4(TotalAmbient+TotalDiffuse+TotalSpecular,1.0); \n"
         "color*=gl_FrontMaterial.emission; \n"

         "vec3 reflection_color=textureCube(envMap,R).rgb; \n"
         "color=mix(color,reflection_color,0.05); \n"

	      "const float kTransparency=0.4; \n"

         "vec3 shadowUV=gl_TexCoord[0].xyz/gl_TexCoord[0].q; \n"
	      "float mapScale=1.0/4096.0; \n"

         "vec4 shadowColor=shadow2D(shadowMap,shadowUV); \n"

         "for(int i=1;i<11;i++){ \n"
	            //"shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(i*mapScale,i*mapScale,0)); \n"
	            //"shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(i*mapScale,-i*mapScale,0)); \n"
	            //"shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(-i*mapScale,i*mapScale,0)); \n"
	            //"shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(-i*mapScale,-i*mapScale,0)); \n"
               "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(i*mapScale,0,0)); \n"
	            "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(-i*mapScale,0,0)); \n"
	            "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(0,i*mapScale,0)); \n"
	            "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(0,-i*mapScale,0)); \n"
         "} \n"

	      "shadowColor=shadowColor/71.0; \n"

	      "shadowColor+=kTransparency; \n"
	      "shadowColor=clamp(shadowColor,0.0,1.0); \n"

	      "if(shadowUV.x>=0.0&&shadowUV.y>=0.0&&shadowUV.x<=1.0&&shadowUV.y<=1.0) \n"
	      "{ \n"
		         "gl_FragColor=color*shadowColor; \n"
	      "} \n"

	      "else \n"
	      "{ \n"
		         "gl_FragColor=color; \n"
	      "} \n"
   "} \n";

//////////////////////////////////////////////////////////////////
//                   Phong_Texture_PCF Shader                   //
//////////////////////////////////////////////////////////////////

char phong_texture_pcf_vertex[]=
   "varying vec2 texCoord; \n"
	"varying vec3 lightPos; \n"
   "varying vec3 normal; \n"
   "varying vec4 eyePos; \n"

   "void main() \n"
   "{ \n"
	      "gl_Position=ftransform(); \n"

         "texCoord=gl_MultiTexCoord0.xy; \n"
         "eyePos=gl_ModelViewMatrix*gl_Vertex; \n"
         "lightPos=gl_LightSource[1].position.xyz; \n"
         "normal=vec3(gl_NormalMatrix*gl_Normal); \n"

         "gl_TexCoord[0].s=dot(eyePos,gl_EyePlaneS[0]); \n"
         "gl_TexCoord[0].t=dot(eyePos,gl_EyePlaneT[0]); \n"
         "gl_TexCoord[0].p=dot(eyePos,gl_EyePlaneR[0]); \n"
         "gl_TexCoord[0].q=dot(eyePos,gl_EyePlaneQ[0]); \n"
   "} \n";

char phong_texture_pcf_fragment[]=
   "uniform sampler2DShadow shadowMap; \n"
   "uniform sampler2D baseMap; \n"

   "varying vec2 texCoord; \n"
	"varying vec3 lightPos; \n"
   "varying vec3 normal; \n"
   "varying vec4 eyePos; \n"

   "void main() \n"
   "{ \n"
         "vec3 N=normalize(normal); \n"
         "vec3 L=normalize(lightPos); \n"
         "float NDotL=max(dot(N,L),0.0); \n"

         "vec3 V=normalize(eyePos.xyz); \n"
         "vec3 R=reflect(V,N); \n"
         "float RDotL=max(dot(R,L),0.0); \n"

         "vec3 baseColor=vec3(texture2D(baseMap,texCoord)); \n"

         "vec3 TotalAmbient=gl_LightSource[1].ambient.rgb*gl_FrontMaterial.ambient.rgb*baseColor; \n"
         "vec3 TotalDiffuse=gl_LightSource[1].diffuse.rgb*gl_FrontMaterial.diffuse.rgb*baseColor*NDotL; \n"
         "vec3 TotalSpecular=gl_LightSource[1].specular.rgb*gl_FrontMaterial.specular.rgb*pow(NDotL,gl_FrontMaterial.shininess); \n"

         "vec4 color=vec4(TotalAmbient+TotalDiffuse+TotalSpecular,1.0); \n"
         "color*=gl_FrontMaterial.emission; \n"

	      "const float kTransparency=0.4; \n"     

         "vec3 shadowUV=gl_TexCoord[0].xyz/gl_TexCoord[0].q; \n"
	      "float mapScale=1.0/4096.0; \n"

         "vec4 shadowColor=shadow2D(shadowMap,shadowUV); \n"

         "for(int i=1;i<11;i++){ \n"
	         //"shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(i*mapScale,i*mapScale,0)); \n"
	         //"shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(i*mapScale,-i*mapScale,0)); \n"
	         //"shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(-i*mapScale,i*mapScale,0)); \n"
	         //"shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(-i*mapScale,-i*mapScale,0)); \n"
            "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(i*mapScale,0,0)); \n"
	         "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(-i*mapScale,0,0)); \n"
	         "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(0,i*mapScale,0)); \n"
	         "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(0,-i*mapScale,0)); \n"
         "} \n"

	      "shadowColor=shadowColor/71.0; \n"

	      "shadowColor+=kTransparency; \n"
	      "shadowColor=clamp(shadowColor,0.0,1.0); \n"

	      "if(shadowUV.x>=0.0&&shadowUV.y>=0.0&&shadowUV.x<=1.0&&shadowUV.y<=1.0) \n"
	      "{ \n"
		      "gl_FragColor=color*shadowColor; \n"
	      "} \n"

	      "else \n"
	      "{ \n"
		      "gl_FragColor=color; \n"
	      "} \n"
   "} \n";

//////////////////////////////////////////////////////////////////
//               Phong_Texture_Reflection Shader                //
//////////////////////////////////////////////////////////////////

char phong_texture_reflection_vertex[]=
   "varying vec2 texCoord; \n"
	"varying vec3 lightPos; \n"
   "varying vec3 normal; \n"
   "varying vec3 eyePos; \n"

   "void main() \n"
   "{ \n"
	      "gl_Position=ftransform(); \n"

         "texCoord=gl_MultiTexCoord0.xy; \n"
         "eyePos=vec3(gl_ModelViewMatrix*gl_Vertex); \n"
         "lightPos=gl_LightSource[1].position.xyz; \n"
         "normal=vec3(gl_NormalMatrix*gl_Normal); \n"
   "} \n";

char phong_texture_reflection_fragment[]=
   "uniform samplerCube envMap; \n"
   "uniform sampler2D baseMap; \n"

   "varying vec2 texCoord; \n"
	"varying vec3 lightPos; \n"
   "varying vec3 normal; \n"
   "varying vec3 eyePos; \n"

   "void main() \n"
   "{ \n"
         "vec3 N=normalize(normal); \n"
         "vec3 L=normalize(lightPos); \n"
         "float NDotL=max(dot(N,L),0.0); \n"

         "vec3 V=normalize(eyePos); \n"
         "vec3 R=reflect(V,N); \n"
         "float RDotL=max(dot(R,L),0.0); \n"

         "vec3 baseColor=vec3(texture2D(baseMap,texCoord)); \n"

         "vec3 TotalAmbient=gl_LightSource[1].ambient.rgb*gl_FrontMaterial.ambient.rgb*baseColor; \n"
         "vec3 TotalDiffuse=gl_LightSource[1].diffuse.rgb*gl_FrontMaterial.diffuse.rgb*baseColor*NDotL; \n"
         "vec3 TotalSpecular=gl_LightSource[1].specular.rgb*gl_FrontMaterial.specular.rgb*pow(NDotL,gl_FrontMaterial.shininess); \n"

         "vec3 color=TotalAmbient+TotalDiffuse+TotalSpecular; \n"
         "color*=gl_FrontMaterial.emission; \n"

         "vec3 reflection_color=textureCube(envMap,R).rgb; \n"
         "color=mix(color,reflection_color,0.05); \n"

         "gl_FragColor=vec4(color,1.0); \n"
   "} \n";

//////////////////////////////////////////////////////////////////
//                 Phong_PCF_Reflection Shader                  //
//////////////////////////////////////////////////////////////////

char phong_pcf_reflection_vertex[]=
   "varying vec2 texCoord; \n"
	"varying vec3 lightPos; \n"
   "varying vec3 normal; \n"
   "varying vec4 eyePos; \n"

   "void main() \n"
   "{ \n"
	      "gl_Position=ftransform(); \n"

         "texCoord=gl_MultiTexCoord0.xy; \n"
         "eyePos=gl_ModelViewMatrix*gl_Vertex; \n"
         "lightPos=gl_LightSource[1].position.xyz; \n"
         "normal=vec3(gl_NormalMatrix*gl_Normal); \n"

         "gl_TexCoord[0].s=dot(eyePos,gl_EyePlaneS[0]); \n"
         "gl_TexCoord[0].t=dot(eyePos,gl_EyePlaneT[0]); \n"
         "gl_TexCoord[0].p=dot(eyePos,gl_EyePlaneR[0]); \n"
         "gl_TexCoord[0].q=dot(eyePos,gl_EyePlaneQ[0]); \n"
   "} \n";

char phong_pcf_reflection_fragment[]=
   "uniform sampler2DShadow shadowMap; \n"
   "uniform samplerCube envMap; \n"

   "varying vec2 texCoord; \n"
	"varying vec3 lightPos; \n"
   "varying vec3 normal; \n"
   "varying vec4 eyePos; \n"

   "void main() \n"
   "{ \n"
         "vec3 N=normalize(normal); \n"
         "vec3 L=normalize(lightPos); \n"
         "float NDotL=max(dot(N,L),0.0); \n"

         "vec3 V=normalize(eyePos.xyz); \n"
         "vec3 R=reflect(V,N); \n"
         "float RDotL=max(dot(R,L),0.0); \n"

         "vec3 TotalAmbient=gl_LightSource[1].ambient.rgb*gl_FrontMaterial.ambient.rgb; \n"
         "vec3 TotalDiffuse=gl_LightSource[1].diffuse.rgb*gl_FrontMaterial.diffuse.rgb*NDotL; \n"
         "vec3 TotalSpecular=gl_LightSource[1].specular.rgb*gl_FrontMaterial.specular.rgb*pow(RDotL,gl_FrontMaterial.shininess); \n"

         "vec3 base=TotalAmbient+TotalDiffuse+TotalSpecular; \n"

         "vec3 reflection=textureCube(envMap,R).rgb; \n"

         "vec4 color=vec4(mix(base,reflection,0.05),1.0); \n"

         "color*=gl_FrontMaterial.emission; \n"

	      "const float kTransparency=0.5; \n"

         "vec3 shadowUV=gl_TexCoord[0].xyz/gl_TexCoord[0].q; \n"
	      "float mapScale=1.0/4096.0; \n"

         "vec4 shadowColor=shadow2D(shadowMap,shadowUV); \n"

         "for(int i=1;i<11;i++){ \n"
	            //"shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(i*mapScale,i*mapScale,0)); \n"
	            //"shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(i*mapScale,-i*mapScale,0)); \n"
	            //"shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(-i*mapScale,i*mapScale,0)); \n"
	            //"shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(-i*mapScale,-i*mapScale,0)); \n"
               "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(i*mapScale,0,0)); \n"
	            "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(-i*mapScale,0,0)); \n"
	            "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(0,i*mapScale,0)); \n"
	            "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(0,-i*mapScale,0)); \n"
         "} \n"

	      "shadowColor=shadowColor/71.0; \n"

	      "shadowColor+=kTransparency; \n"
	      "shadowColor=clamp(shadowColor,0.0,1.0); \n"

	      "if(shadowUV.x>=0.0&&shadowUV.y>=0.0&&shadowUV.x<=1.0&&shadowUV.y<=1.0) \n"
	      "{ \n"
		         "gl_FragColor=color*shadowColor; \n"
	      "} \n"

	      "else \n"
	      "{ \n"
		         "gl_FragColor=color; \n"
	      "} \n"
   "} \n";

//////////////////////////////////////////////////////////////////
//                Texture_PCF_Reflection Shader                 //
//////////////////////////////////////////////////////////////////

char texture_pcf_reflection_vertex[]=
   "varying vec3 TotalAmbient; \n"
   "varying vec3 TotalDiffuse; \n"
   "varying vec3 TotalSpecular; \n"

   "varying vec3 R; \n"

   "void main() \n"
   "{ \n"
	      "gl_Position=ftransform(); \n"

         "vec4 eyePos=gl_ModelViewMatrix*gl_Vertex; \n"
         "vec3 lightPos=gl_LightSource[1].position.xyz; \n"
         "vec3 normal=vec3(gl_NormalMatrix*gl_Normal); \n"

         "vec3 N=normalize(normal); \n"
         "vec3 L=normalize(lightPos); \n"
         "float NDotL=max(0.0,dot(N,L)); \n"
   
         "vec3 V=normalize(eyePos.xyz); \n"
         "R=reflect(V,N); \n"
         //"float RDotL=max(0.0,dot(R,L)); \n"   
 
         "TotalAmbient=gl_LightSource[1].ambient.rgb*gl_FrontMaterial.ambient.rgb; \n"
         "TotalDiffuse=gl_LightSource[1].diffuse.rgb*gl_FrontMaterial.diffuse.rgb*NDotL; \n"
         "TotalSpecular=gl_LightSource[1].specular.rgb*gl_FrontMaterial.specular.rgb*pow(NDotL,gl_FrontMaterial.shininess); \n"

         "gl_TexCoord[0].s=dot(eyePos,gl_EyePlaneS[0]); \n"
         "gl_TexCoord[0].t=dot(eyePos,gl_EyePlaneT[0]); \n"
         "gl_TexCoord[0].p=dot(eyePos,gl_EyePlaneR[0]); \n"
         "gl_TexCoord[0].q=dot(eyePos,gl_EyePlaneQ[0]); \n"
   "} \n";

char texture_pcf_reflection_fragment[]=
   "uniform samplerCube envMap; \n"
   "uniform sampler2DShadow shadowMap; \n"
   "uniform sampler2D baseMap; \n"

	"varying vec3 TotalAmbient; \n"
   "varying vec3 TotalDiffuse; \n"
   "varying vec3 TotalSpecular; \n"

   "varying vec3 R; \n"

   "void main() \n"
   "{ \n"
         "vec3 baseColor=vec3(texture2D(baseMap,texCoord)); \n"

         "TotalAmbient*=baseColor; \n"
         "TotalDiffuse*=baseColor; \n"

         "vec4 color=vec4(TotalAmbient+TotalDiffuse+TotalSpecular,1.0); \n"
         "color*=gl_FrontMaterial.emission; \n"

         "vec3 reflection_color=textureCube(envMap,R).rgb; \n"
         "color=mix(color,reflection_color,0.05); \n"

	      "const float kTransparency=0.5; \n"

         "vec3 shadowUV=gl_TexCoord[0].xyz/gl_TexCoord[0].q; \n"
	      "float mapScale=1.0/4096.0; \n"

         "vec4 shadowColor=shadow2D(shadowMap,shadowUV); \n"

         "for(int i=1;i<11;i++){ \n"
	            //"shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(i*mapScale,i*mapScale,0)); \n"
	            //"shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(i*mapScale,-i*mapScale,0)); \n"
	            //"shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(-i*mapScale,i*mapScale,0)); \n"
	            //"shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(-i*mapScale,-i*mapScale,0)); \n"
               "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(i*mapScale,0,0)); \n"
	            "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(-i*mapScale,0,0)); \n"
	            "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(0,i*mapScale,0)); \n"
	            "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(0,-i*mapScale,0)); \n"
         "} \n"

	      "shadowColor=shadowColor/71.0; \n"

	      "shadowColor+=kTransparency; \n"
	      "shadowColor=clamp(shadowColor,0.0,1.0); \n"

	      "if(shadowUV.x>=0.0&&shadowUV.y>=0.0&&shadowUV.x<=1.0&&shadowUV.y<=1.0) \n"
	      "{ \n"
		         "gl_FragColor=color*shadowColor; \n"
	      "} \n"

	      "else \n"
	      "{ \n"
		         "gl_FragColor=color; \n"
	      "} \n"
   "} \n";

//////////////////////////////////////////////////////////////////
//             Phong_Texture_PCF_Reflection Shader              //
//////////////////////////////////////////////////////////////////

char phong_texture_pcf_reflection_vertex[]=
   "varying vec2 texCoord; \n"
	"varying vec3 lightPos; \n"
   "varying vec3 normal; \n"
   "varying vec4 eyePos; \n"

   "void main() \n"
   "{ \n"
	      "gl_Position=ftransform(); \n"

         "texCoord=gl_MultiTexCoord0.xy; \n"
         "eyePos=gl_ModelViewMatrix*gl_Vertex; \n"
         "lightPos=gl_LightSource[1].position.xyz; \n"
         "normal=vec3(gl_NormalMatrix*gl_Normal); \n"

         "gl_TexCoord[0].s=dot(eyePos,gl_EyePlaneS[0]); \n"
         "gl_TexCoord[0].t=dot(eyePos,gl_EyePlaneT[0]); \n"
         "gl_TexCoord[0].p=dot(eyePos,gl_EyePlaneR[0]); \n"
         "gl_TexCoord[0].q=dot(eyePos,gl_EyePlaneQ[0]); \n"
   "} \n";

char phong_texture_pcf_reflection_fragment[]=
   "uniform sampler2DShadow shadowMap; \n"
   "uniform samplerCube envMap; \n"
   "uniform sampler2D baseMap; \n"

   "varying vec2 texCoord; \n"
	"varying vec3 lightPos; \n"
   "varying vec3 normal; \n"
   "varying vec4 eyePos; \n"

   "void main() \n"
   "{ \n"
         "vec3 N=normalize(normal); \n"
         "vec3 L=normalize(lightPos); \n"
         "float NDotL=max(dot(N,L),0.0); \n"

         "vec3 V=normalize(eyePos.xyz); \n"
         "vec3 R=reflect(V,N); \n"
         "float RDotL=max(dot(R,L),0.0); \n"

         "vec3 texture=vec3(texture2D(baseMap,texCoord)); \n"

         "vec3 TotalAmbient=gl_LightSource[1].ambient.rgb*gl_FrontMaterial.ambient.rgb*texture; \n"
         "vec3 TotalDiffuse=gl_LightSource[1].diffuse.rgb*gl_FrontMaterial.diffuse.rgb*texture*NDotL; \n"
         "vec3 TotalSpecular=gl_LightSource[1].specular.rgb*gl_FrontMaterial.specular.rgb*pow(RDotL,gl_FrontMaterial.shininess); \n"

         "vec3 base=TotalAmbient+TotalDiffuse+TotalSpecular; \n"

         "vec3 reflection=textureCube(envMap,R).rgb; \n"

         "vec4 color=vec4(mix(base,reflection,0.05),1.0); \n"

         "color*=gl_FrontMaterial.emission; \n"

	     "const float kTransparency=0.5; \n"

         "vec3 shadowUV=gl_TexCoord[0].xyz/gl_TexCoord[0].q; \n"
	     "float mapScale=1.0/4096.0; \n"

         "vec4 shadowColor=shadow2D(shadowMap,shadowUV); \n"

         "for(int i=1;i<11;i++){ \n"
	            //"shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(i*mapScale,i*mapScale,0)); \n"
	            //"shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(i*mapScale,-i*mapScale,0)); \n"
	            //"shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(-i*mapScale,i*mapScale,0)); \n"
	            //"shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(-i*mapScale,-i*mapScale,0)); \n"
               "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(i*mapScale,0,0)); \n"
	            "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(-i*mapScale,0,0)); \n"
	            "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(0,i*mapScale,0)); \n"
	            "shadowColor+=shadow2D(shadowMap,shadowUV.xyz+vec3(0,-i*mapScale,0)); \n"
         "} \n"

	      "shadowColor=shadowColor/71.0; \n"

	      "shadowColor+=kTransparency; \n"
	      "shadowColor=clamp(shadowColor,0.0,1.0); \n"

	      "if(shadowUV.x>=0.0&&shadowUV.y>=0.0&&shadowUV.x<=1.0&&shadowUV.y<=1.0) \n"
	      "{ \n"
		         "gl_FragColor=color*shadowColor; \n"
	      "} \n"

	      "else \n"
	      "{ \n"
		         "gl_FragColor=color; \n"
	      "} \n"
   "} \n";

#endif