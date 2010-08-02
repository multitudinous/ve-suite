/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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

#ifndef MSMRT_CALLBACKS_H
#define MSMRT_CALLBACKS_H

// --- OSG Includes --- //
#include <osg/Camera>
#include <osg/GLExtensions>
#include <osg/FrameBufferObject>

#include <osgUtil/CullVisitor>

//Define some OpenGL constants for FBOs that OSG doesn't define/use
#ifndef GL_READ_FRAMEBUFFER
#define GL_READ_FRAMEBUFFER 0x8CA8
#endif
#ifndef GL_DRAW_FRAMEBUFFER
#define GL_DRAW_FRAMEBUFFER 0x8CA9
#endif
#ifndef GL_COLOR_ATTACHMENT0
#define GL_COLOR_ATTACHMENT0 0x8CE0
#endif
#ifndef GL_COLOR_ATTACHMENT1
#define GL_COLOR_ATTACHMENT1 ( GL_COLOR_ATTACHMENT0 + 1 )
#endif
#ifndef GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME
#define GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME 0x8CD1
#endif
#ifndef GL_FRAMEBUFFER_BINDING
#define GL_FRAMEBUFFER_BINDING 0x8CA6
#endif
#ifndef GL_DRAW_FRAMEBUFFER_BINDING
#define GL_DRAW_FRAMEBUFFER_BINDING GL_FRAMEBUFFER_BINDING
#endif
#ifndef GL_READ_FRAMEBUFFER_BINDING
#define GL_READ_FRAMEBUFFER_BINDING 0x8CAA
#endif
#ifndef GL_RENDERBUFFER
#define GL_RENDERBUFFER 0x8D41
#endif

class MSMRTCallback : public osg::Camera::DrawCallback
{
public:
    ///
    MSMRTCallback( osg::Camera* cam )
        :
        _cam( cam )
    {
        ;
    }

    ///
    virtual void operator()( osg::RenderInfo& renderInfo ) const
    {
        osg::State& state = *renderInfo.getState();
        const unsigned int ctx = state.getContextID();
        osg::FBOExtensions* fboExt = osg::FBOExtensions::instance( ctx, true );

        PerContextInfo& ctxInfo( _contextInfo[ ctx ] );
        if( ctxInfo.__glGetFramebufferAttachmentParameteriv == NULL )
        {
            //Initialize function pointer for FBO query
            osg::setGLExtensionFuncPtr(
                ctxInfo.__glGetFramebufferAttachmentParameteriv,
                "glGetFramebufferAttachmentParameteriv" );
        }

        const GLint width = _cam->getViewport()->width();
        const GLint height = _cam->getViewport()->height();

#if 0
        //Make sure something is actually bound
        GLint drawFBO( -1 );
        glGetIntegerv( GL_DRAW_FRAMEBUFFER_BINDING, &drawFBO );
#endif

        //BlitFramebuffer blits to all attached color buffers in the draw FBO
        //We only want to blit to attachment1,
        //so aave attachment0 and then unbind it
        GLint destColorTex0( -1 );
        ctxInfo.__glGetFramebufferAttachmentParameteriv(
            GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
            GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME, &destColorTex0 );
        fboExt->glFramebufferTexture2DEXT(
            GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
            GL_TEXTURE_2D, 0, 0 );

        //Verification
        //osg::notify( osg::ALWAYS )
            //<< "Dest " << std::hex << destColorTex0 << std::endl;

        //Set draw and read buffers to attachment1 to read from correct
        //buffer and avoid INVALID_FRAMEBUFFER_OPERATION error
        glDrawBuffer( GL_COLOR_ATTACHMENT1 );
        glReadBuffer( GL_COLOR_ATTACHMENT1 );

        //Blit, from (multisampled read FBO) attachment1 to
        //(non-multisampled draw FBO) attachment1
        fboExt->glBlitFramebufferEXT(
            0, 0, width, height, 0, 0, width, height,
            GL_COLOR_BUFFER_BIT, GL_NEAREST );

        //Restore draw and read buffers
        glDrawBuffer( GL_COLOR_ATTACHMENT0 );
        glReadBuffer( GL_COLOR_ATTACHMENT0 );

        //Restore the draw FBO's attachment0
        fboExt->glFramebufferTexture2DEXT(
            GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
            GL_TEXTURE_2D, destColorTex0, 0 );

        //We disabled FBO unbinding in the RenderStage, so do it ourself here
        fboExt->glBindFramebufferEXT( GL_FRAMEBUFFER_EXT, 0 );
    }

protected:
    ///
    osg::ref_ptr< osg::Camera > _cam;

    ///
    typedef void APIENTRY TglGetFramebufferAttachmentParameteriv(
        GLenum, GLenum, GLenum, GLint* );

    //Each different context could potentially have a different address for
    //the FBO query function. For this reason, keep it in buffered_value
    //and init / index the function pointer on a per-context basis
    //Of course, this wouldn't be necessary if OSG already has this function
    //pointer in FBOExtensions. Or if OSG used something like GLEW
    ///
    struct PerContextInfo
    {
        PerContextInfo()
        {
            __glGetFramebufferAttachmentParameteriv = NULL;
        }

        TglGetFramebufferAttachmentParameteriv* __glGetFramebufferAttachmentParameteriv;
    };

    ///
    mutable osg::buffered_object< PerContextInfo > _contextInfo;
};


//RenderStage unbinds FBOs before executing post-draw callbacks
//The only way I know of to access the RenderStage (to disable this unbinding)
//is with a cull callback
class KeepFBOsBoundCallback : public osg::NodeCallback
{
public:
    ///
    KeepFBOsBoundCallback( unsigned int const& numViewports )
        :
        m_numViewports( numViewports )
    {
        ;
    }

    ///
    virtual void operator()( osg::Node* node, osg::NodeVisitor* nv )
    {
        if( nv->getVisitorType() != osg::NodeVisitor::CULL_VISITOR )
        {
            traverse( node, nv );
            return;
        }

        //Should only see this message once
        //(or twice, for osgViewer) per cull thread
        osg::notify( osg::ALWAYS ) << "In KeepFBOsBoundCallback, cull traversal"
                                   << std::endl;

        //Get the current RenderStage and prevent it from unbinding
        //the FBOs just before our post-draw MSMRTCallback is executed
        //We need them bound in our callback so we can execute
        //another glBlitFramebuffer
        //After the blit, MSMRTCallback unbinds the FBOs
        osgUtil::CullVisitor* cv = dynamic_cast< osgUtil::CullVisitor* >( nv );
        //Don't use getRenderStage(); it returns the _root_ RenderStage
        //osgUtil::RenderStage* rs = cv->getRenderStage();
        osgUtil::RenderStage* rs = cv->getCurrentRenderBin()->getStage();
        rs->setDisableFboAfterRender( false );

        traverse( node, nv );

        --m_numViewports;

        //The cull visitor only needs to execute once to tell the RenderStage
        //to keep the FBOs bound, so remove the cull callback now
        if( m_numViewports == 0 )
        {
            node->setCullCallback( NULL );
        }
    }

protected:

private:
    ///
    unsigned int m_numViewports;

};

#endif //MSMRT_CALLBACKS_H
