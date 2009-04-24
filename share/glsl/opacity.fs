uniform sampler2D tex;
uniform float opacityVal;

void main()
{
    vec4 color = texture2D(tex, gl_TexCoord[0].st);
    //gl_Color
    color.a = opacityVal;
	gl_FragColor = color;
}
