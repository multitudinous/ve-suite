/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

// --- My Includes --- //
#include "DepthOfFieldTechnique.h"
#include "CameraEntity.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/ResourceManager.h>

// --- OSG Includes --- //
#include <osg/Texture2D>

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
DepthOfFieldTechnique::DepthOfFieldTechnique( cpt::CameraEntity* cameraEntity )
:
ves::xplorer::scenegraph::Technique(),
mCameraEntity( cameraEntity )
{
    DefinePasses();
}
////////////////////////////////////////////////////////////////////////////////
DepthOfFieldTechnique::~DepthOfFieldTechnique()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void DepthOfFieldTechnique::DefinePasses()
{
    //Implement pass #1
    {
        FirstPass();
    }
    //Implement pass #2
    {
        SecondPass();
    }
    //Implement pass #3
    {
        ThirdPass();
    }
}
////////////////////////////////////////////////////////////////////////////////
void DepthOfFieldTechnique::FirstPass()
{
    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();

    AddPass( stateset.get() );
}
////////////////////////////////////////////////////////////////////////////////
void DepthOfFieldTechnique::SecondPass()
{
    std::string DoFSecondPassVertexSource =
    "varying float Depth; \n"

    "void main() \n"
    "{ \n"
        "vec4 PosWV = gl_ModelViewMatrix * gl_Vertex; \n"
        "Depth = -PosWV.z; \n"

        "gl_Position = ftransform(); \n"
        "gl_TexCoord[ 0 ] = gl_MultiTexCoord0; \n"
    "} \n";

    std::string DoFSecondPassFragmentSource =
    "#extension GL_ARB_draw_buffers : enable \n"

    "uniform float focalLen; \n"
    "uniform float Zfocus; \n"
    "uniform float maxCoC; \n"

    "uniform sampler2D Tex0; \n"

    "varying float Depth; \n"

    "void main() \n"
    "{ \n"
	    "float Dlens = 1.0; \n"
	    "float scale  = 5.0; \n"
	    "float sceneRange = 35.0; \n"

	    "float pixCoC = abs( Dlens * focalLen * ( Zfocus - Depth ) / \n"
                                    "( Zfocus * ( Depth - focalLen ) ) ); \n"
	    "float blur = clamp( pixCoC * scale / maxCoC, 0.0, 1.0 ); \n"

	    "gl_FragData[ 0 ] = texture2D( Tex0, gl_TexCoord[ 0 ].st ); \n"
	    "gl_FragData[ 1 ] = vec4( Depth / sceneRange, blur, 0, 0 ); \n"
    "} \n";

    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();

    AddPass( stateset.get() );
}
////////////////////////////////////////////////////////////////////////////////
void DepthOfFieldTechnique::ThirdPass()
{
    std::string DoFThirdPassVertexSource =
    "void main() \n"
    "{ \n"
	    "gl_Position = ftransform(); \n"

	    "gl_TexCoord[ 0 ] = gl_MultiTexCoord0; \n"
	    "gl_TexCoord[ 1 ] = gl_MultiTexCoord1; \n"
    "} \n";

    std::string DoFThirdPassFragmentSource =
    "uniform int Width; \n"
    "uniform int Height; \n"

    "uniform float maxCoC; \n"

    "uniform sampler2D Tex0; \n"
    "uniform sampler2D Tex1; \n"

    "void main() \n"
    "{ \n"
        "vec4 colorSum, tapColor; \n"
        "vec2 centerDepthBlur, tapCoord, tapDepthBlur; \n"
	    "float totalContribution, tapContribution; \n"

	    //Poissonian disc distribution
	    "float dx = 1.0 / float( Width ); \n"
	    "float dy = 1.0 / float( Height ); \n"

	    "vec2 filterTaps[ 12 ]; \n"
	    "filterTaps[ 0 ]  = vec2( -0.326212 * dx, -0.405810 * dy ); \n"
	    "filterTaps[ 1 ]  = vec2( -0.840144 * dx, -0.073580 * dy ); \n"
	    "filterTaps[ 2 ]  = vec2( -0.695914 * dx,  0.457137 * dy ); \n"
	    "filterTaps[ 3 ]  = vec2( -0.203345 * dx,  0.620716 * dy ); \n"
	    "filterTaps[ 4 ]  = vec2(  0.962340 * dx, -0.194983 * dy ); \n"
	    "filterTaps[ 5 ]  = vec2(  0.473434 * dx, -0.480026 * dy ); \n"
	    "filterTaps[ 6 ]  = vec2(  0.519456 * dx,  0.767022 * dy ); \n"
	    "filterTaps[ 7 ]  = vec2(  0.185461 * dx, -0.893124 * dy ); \n"
	    "filterTaps[ 8 ]  = vec2(  0.507431 * dx,  0.064425 * dy ); \n"
	    "filterTaps[ 9 ]  = vec2(  0.896420 * dx,  0.412458 * dy ); \n"
	    "filterTaps[ 10 ] = vec2( -0.321940 * dx, -0.932615 * dy ); \n"
	    "filterTaps[ 11 ] = vec2( -0.791559 * dx, -0.597710 * dy ); \n"

	    //Starting with center sample
	    "colorSum = texture2D( Tex0, gl_TexCoord[ 0 ].st ); \n"
	    "totalContribution = 1.0; \n"
	    "centerDepthBlur = texture2D( Tex1, gl_TexCoord[ 1 ].st ).xy; \n"

	    "float sizeCoC = centerDepthBlur.y * maxCoC; \n"

	    "for( int i = 0; i < 12; ++i ) \n"
        "{ \n"
		    "tapCoord = gl_TexCoord[ 0 ].st + filterTaps[ i ] * sizeCoC; \n"
		    "tapColor = texture2D( Tex0, tapCoord ); \n"
		    "tapDepthBlur = texture2D( Tex1, tapCoord ).xy; \n"
		    "tapContribution = ( tapDepthBlur.x > centerDepthBlur.x ) ? 1.0 : tapDepthBlur.y; \n"
		    "colorSum += tapColor * tapContribution; \n"
		    "totalContribution += tapContribution; \n"
	    "} \n"

	    "gl_FragColor = colorSum / totalContribution; \n"
    "} \n";

    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();

    AddPass( stateset.get() );
}
////////////////////////////////////////////////////////////////////////////////
void DepthOfFieldTechnique::BlurOverlay()
{
    std::string DoFRenderBlurFragmentSource =
    "uniform sampler2D Tex1; \n"

    "void main() \n"
    "{ \n"
	    "float blurAmount = texture2D( Tex1, gl_TexCoord[ 0 ].st ).y; \n"

	    "gl_FragColor = vec4( blurAmount, blurAmount, blurAmount, 1.0 ); \n"
    "} \n";

    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();

    AddPass( stateset.get() );
}
////////////////////////////////////////////////////////////////////////////////
