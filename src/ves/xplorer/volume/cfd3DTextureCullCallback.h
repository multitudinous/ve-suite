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
#ifndef CFD_3D_TEXTURE_UPDATE_CALLBACK_H
#define CFD_3D_TEXTURE_UPDATE_CALLBACK_H
/*!\file cfd3DTextureCullCallback.h
* cfd3DTextureCullCallback API
*/

/*!\class ves::xplorer::volume::cfd3DTextureCullCallback
*
*/
#ifdef _OSG
//#ifdef CFD_USE_SHADERS
namespace osg
{
class Node;
class Texture3D;
class NodeVisitor;
class Viewport;
class FrameStamp;
//class BoundingBox;
}
namespace osgUtil
{
class CullVisitor;
class UpdateVisitor;
}

#include <osg/Node>
#include <osg/NodeCallback>
#include <osg/BoundingBox>
namespace ves
{
namespace xplorer
{
namespace volume
{
class cfdPBufferManager;
class cfdOSGPingPongTexture3D;
}
#include <ves/VEConfig.h>
namespace volume
{
class VE_TEXTURE_BASED_EXPORTS cfd3DTextureCullCallback
            : public osg::NodeCallback
{
public:
    cfd3DTextureCullCallback( osg::Node* subgraph,
                              //osg::Texture3D* texture,
                              unsigned int width,
                              unsigned int height );

    virtual ~cfd3DTextureCullCallback();
    void SetPBuffer( cfdPBufferManager* pbuffer )
    {
        _pbuffer = pbuffer;
    }
    void SetPingPongTextures( unsigned int tPingUint,
                              osg::Node* ping,
                              unsigned int tPongUint,
                              osg::Node* pong );
    virtual void operator()( osg::Node* node, osg::NodeVisitor* nv );

    void preRender( osg::Node& node, osgUtil::CullVisitor& cv );
    cfdOSGPingPongTexture3D* GetPingPonger();
    osg::Node* subgraph()
    {
        return _subgraph.get();
    }

protected:
    osg::ref_ptr<osg::Node> _subgraph;
    unsigned int _w;
    unsigned int _h;
    unsigned int _count;
    cfdPBufferManager* _pbuffer;
    cfdOSGPingPongTexture3D* _pingPonger;
    osg::ref_ptr<osg::Texture3D> _textureToUpdate;
    osg::ref_ptr<osg::Node> _current;
    osg::ref_ptr<osg::Node> _previous;
    osg::ref_ptr<osg::StateSet> _localState;
    osgUtil::UpdateVisitor* _uniformUpdater;
    osg::ref_ptr<osg::FrameStamp> _fs;
};
}
}
}
#endif//_OSG
#endif //CFD_3D_TEXTURE_UPDATE_CALLBACK_H
