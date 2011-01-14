
// shaders/gl2/ffp-declarations.common
// shaders/gl2/ffp-declarations.vs

// Copyright (c) 2010 Skew Matrix Software. All rights reserved.
// gl2/ffp-texgen.vs


void computeTexGen()
{
    bdfx_outTexCoord[ 0 ] = bdfx_multiTexCoord0;
    bdfx_outTexCoord[ 1 ] = bdfx_multiTexCoord1;
    bdfx_outTexCoord[ 2 ] = bdfx_multiTexCoord2;
    bdfx_outTexCoord[ 3 ] = bdfx_multiTexCoord3;

    int idx;
    for( idx=0; idx<bdfx_maxTextureCoords; idx++ )
    {
        if( bdfx_texGen[ idx ] == 0 ) continue; // none
        else if( bdfx_texGen[ idx ] == GL_EYE_LINEAR ) // EYE LINEAR
        {
            /*
            // from orange book, listing 9.22
            gl_TexCoord[i].s = dot(ecPosition, gl_EyePlaneS[i]);
            gl_TexCoord[i].t = dot(ecPosition, gl_EyePlaneT[i]);
            gl_TexCoord[i].p = dot(ecPosition, gl_EyePlaneR[i]);
            gl_TexCoord[i].q = dot(ecPosition, gl_EyePlaneQ[i]);
            */
            // ecPosition   == bdfx_eyeVertex (precalculated by ffp-eyecoords)
            // gl_EyePlane* == bdfx_eyePlane* (preloaded by ShaderModuleVisitor)

            // bdfx_eyeVertex relies on ffp-eyecoords-on.vs being run, no way to enforce this from here
            if( bdfx_eyeAbsolute == 1 )
            {
                bdfx_outTexCoord[idx].s = dot(bdfx_eyeVertex, bdfx_eyePlaneS[idx]);
                bdfx_outTexCoord[idx].t = dot(bdfx_eyeVertex, bdfx_eyePlaneT[idx]);
                bdfx_outTexCoord[idx].p = dot(bdfx_eyeVertex, bdfx_eyePlaneR[idx]);
                bdfx_outTexCoord[idx].q = dot(bdfx_eyeVertex, bdfx_eyePlaneQ[idx]);
            }
            else
            {
                bdfx_outTexCoord[idx].s = dot(bdfx_eyeVertex, ( bdfx_eyePlaneS[idx] * osg_ViewMatrixInverse ) );
                bdfx_outTexCoord[idx].t = dot(bdfx_eyeVertex, ( bdfx_eyePlaneT[idx] * osg_ViewMatrixInverse ) );
                bdfx_outTexCoord[idx].p = dot(bdfx_eyeVertex, ( bdfx_eyePlaneR[idx] * osg_ViewMatrixInverse ) );
                bdfx_outTexCoord[idx].q = dot(bdfx_eyeVertex, ( bdfx_eyePlaneQ[idx] * osg_ViewMatrixInverse ) );
            }

        } // GL_EYE_LINEAR
        else if( bdfx_texGen[ idx ] == GL_OBJECT_LINEAR ) // OBJECT LINEAR
        {
            // cf. orange book, listing 9.22
            bdfx_outTexCoord[idx].s = dot(bdfx_vertex, bdfx_objectPlaneS[idx]);
            bdfx_outTexCoord[idx].t = dot(bdfx_vertex, bdfx_objectPlaneT[idx]);
            bdfx_outTexCoord[idx].p = dot(bdfx_vertex, bdfx_objectPlaneR[idx]);
            bdfx_outTexCoord[idx].q = dot(bdfx_vertex, bdfx_objectPlaneQ[idx]);
        } // GL_OBJECT_LINEAR
        else if( bdfx_texGen[ idx ] == GL_SPHERE_MAP ) // sphere
        {
            vec3 u = normalize( bdfx_eyeVertex.xyz );
            vec3 r = u - 2.0 * bdfx_eyeNormal * dot( u, bdfx_eyeNormal );
            float m = 2.0 * sqrt( r.x*r.x + r.y*r.y + (r.z+1.0)*(r.z+1.0) );
            bdfx_outTexCoord[ idx ] = vec4( r.x/m + 0.5, r.y/m + 0.5, 0., 1. );
        } // GL_SPHERE_MAP
        // both blocks below rely on ecPosition3
        vec3 ecPosition3;
        ecPosition3 = (vec3(bdfx_eyeVertex)) / bdfx_eyeVertex.w;
        if( bdfx_texGen[ idx ] == GL_NORMAL_MAP ) // NORMAL MAP
        {
            bdfx_outTexCoord[ idx ] = vec4(bdfx_eyeNormal, 1.0);
        } // GL_NORMAL_MAP
        if( bdfx_texGen[ idx ] == GL_REFLECTION_MAP ) // REFLECTION MAP
        {
            // cf. orange book, listing 9.21
            vec3 u;
            u = normalize(ecPosition3); // this could be hoisted out to where we compute ecPosition3 above
            bdfx_outTexCoord[ idx ] = vec4(reflect(u, bdfx_eyeNormal), 0.0); // explicitly promote vec3 up to vec4 by appending 0
        } // GL_REFLECTION_MAP
    }
}

// END gl2/ffp-texgen.vs

