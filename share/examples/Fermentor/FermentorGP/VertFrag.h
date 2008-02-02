#ifndef VERT_FRAG_H
#define VERT_FRAG_H

//////////////////////////////////////////////////////////////////
//                       Phong Shader                           //
//////////////////////////////////////////////////////////////////

    char phongVertex[]= 
        "varying vec3 color; \n"
        "varying vec3 lightPos; \n"
        "varying vec3 objPos; \n"
        "varying vec3 eyePos; \n"
        "varying vec3 normal; \n"
        " \n"
        "void main() \n"
        "{ \n"
        "     gl_Position=ftransform(); \n"
        " \n"      
        "     color=gl_Color.xyz; \n"
        "     objPos=gl_Vertex.xyz; \n"
        "     eyePos=vec3(gl_ModelViewMatrix*gl_Vertex); \n"
        "     lightPos=gl_LightSource[0].position.xyz; \n"
        "     normal=vec3(gl_NormalMatrix*gl_Normal); \n"
        "} \n";

    char phongFragment[]=
        "uniform vec3 ambientMaterial; \n"
        "uniform vec3 diffuseMaterial; \n"
        "uniform vec3 specularMaterial; \n"
        "uniform float specularPower; \n"
        " \n"
        "varying vec3 color; \n"
        "varying vec3 lightPos; \n"
        "varying vec3 objPos; \n"
        "varying vec3 eyePos; \n"
        "varying vec3 normal; \n"
        " \n"
        "void main() \n"
        "{ \n"
        "vec3 N=normalize(normal); \n"
        "vec3 L=normalize(lightPos); \n"
        "float NDotL=max(dot(N,L),0.0); \n"
        " \n"
        "vec3 V=normalize(eyePos); \n"
        "vec3 R=reflect(V,N); \n"
        "float RDotL=max(dot(R,L),0.0); \n"
        " \n"
        "vec3 TotalAmbient=ambientMaterial*ambientMaterial; \n" 
        "vec3 TotalDiffuse=diffuseMaterial*diffuseMaterial*NDotL; \n" 
        "vec3 TotalSpecular=specularMaterial*specularMaterial*pow(RDotL,specularPower); \n"
        " \n"
        "gl_FragColor=vec4(TotalAmbient+TotalDiffuse+TotalSpecular,1.0); \n"
        "} \n";

    //////////////////////////////////////////////////////////////////
    //                       X-Ray Shader                           //
    //////////////////////////////////////////////////////////////////

    char xrayVertex[]=
        "varying vec3 N; \n"
        "varying vec3 I; \n"
        "varying vec4 currentColor; \n"
        " \n"
        "void main() \n"
        "{ \n"
        "vec4 P=gl_ModelViewMatrix*gl_Vertex; \n"
        "I=P.xyz; \n"
        "N=gl_NormalMatrix*gl_Normal; \n"
        "currentColor=gl_Color; \n"
        "gl_Position=ftransform(); \n"
        "} \n";

    char xrayFragment[]=
        "varying vec3 N; \n"
        "varying vec3 I; \n"
        "varying vec4 currentColor; \n"	
        " \n"
        "void main() \n"
        "{ \n"
        "float opac=dot(normalize(-N),normalize(-I)); \n"
        "opac=abs(opac); \n"
        "opac=1.0-pow(opac,3.0); \n"
        "vec4 Cs=currentColor; \n"
        "gl_FragColor=opac*Cs; \n"
        "} \n";              

#endif //VERT_FRAG_H
