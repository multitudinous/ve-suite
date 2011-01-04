/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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

#ifndef CFD_VOLUME_VIZ_NODE_HANDLER_H
#define CFD_VOLUME_VIZ_NODE_HANDLER_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/BoundingBox>

namespace osg
{
class Group;
class Switch;
class TexGenNode;
}

// --- C/C++ Includes --- //
#include <string>
#include <map>

namespace ves
{
namespace xplorer
{
namespace volume
{
class cfdTextureManager;
class cfdOSGShaderManager;

/*!\file cfdVolumeVisNodeHandler.h
 * cfdVolumeVisNodeHandler API
 */

/*!\class ves::xplorer::volume::cfdVolumeVisNodeHandler
 *
 */
class VE_TEXTURE_BASED_EXPORTS cfdVolumeVisNodeHandler
{
public:
    ///Constructor
    cfdVolumeVisNodeHandler();
    ///Copy Constructor
    ///\param vvnh cfdVolumeVisNodeHandler to copy
    cfdVolumeVisNodeHandler( const cfdVolumeVisNodeHandler& vvnh );
    ///Destructor
    virtual ~cfdVolumeVisNodeHandler();

    ///Set the top level switch node from cfdVolumeVisualization
    ///\param vvn osg::Switch
    void SetSwitchNode( osg::Switch* vvn );

    ///Set the attachement node from cfdVolumeVisualization
    ///\param attachNode osg::Group from cfdVolumeVisualization
    void SetAttachNode( osg::Group* attachNode );
    ///Set the center of the data
    ///\param center The center of the data
    void SetCenter( osg::Vec3f center );
    ///Set the scale
    ///\param scale The scale of the texture
    ///\param isInverted Is the scale inverted
    void SetTextureScale( float* scale, bool isInverted = true );

    ///Set the current cfdTextureManager
    ///\param tm cfdTextureManager pointer
    void SetTextureManager( cfdTextureManager* tm );
    ///Set the bounding box of the data
    ///\param bbox The bounds of the data
    void SetBoundingBox( float* bbox );
    ///Set the bounding box name for the graph
    ///\param name Name for the bbox node
    void SetBoundingBoxName( std::string name );
    ///Set the dectorator node name
    ///\param name Name for the decorator name
    void SetDecoratorName( std::string name );
    ///Update which scalar shader is active
    ///\param name The shader name
    void SetActiveShader( std::string name );
    ///Is this algorithm active
    bool IsThisActive();
    ///Initialize parameters
    virtual void Init();

    ///Turn on the visual bounding box
    void TurnOnBBox();
    ///Turn off the visual bbox
    void TurnOffBBox();

    ///Enable the active shader algorithm
    void EnableDecorator();

    ///Add new shader manager.
    ///\param name The name of the shader
    ///\param newShader The shader manager
    void AddShaderManager( std::string name,
                           ves::xplorer::volume::cfdOSGShaderManager* newShader,
                           bool isScalar = true );

    ///Get the active shader
    std::string GetActiveShaderName();

    ///Get a pointer to the active shader
    ves::xplorer::volume::cfdOSGShaderManager* GetActiveShader();

    ///Get a shader manager
    ///\param name The name of the shader
    ves::xplorer::volume::cfdOSGShaderManager* GetShaderManager( std::string name );

    cfdVolumeVisNodeHandler& operator=( const cfdVolumeVisNodeHandler& vvnh );

protected:
    ///Set up the geometry for the bounding box
    void _createVisualBBox();
    ///Set up the stateset for the decorator
    virtual void _setUpDecorator() = 0;
    ///Apply the texture matrix
    virtual void _applyTextureMatrix() = 0;
    ///Update the auto-gen texture unit
    ///\param unit The texture unit
    virtual void _updateTexGenUnit( unsigned int unit = 0 );
    ///Create the auto texture generation node
    void _createTexGenNode();
    unsigned int _whichChildIsThis;///<Index of the child in the shader switch
    unsigned int _whichTexture;///<Index of the current timestep
    bool _autoTexGen;///<Use auto texture coordinate generation
    cfdTextureManager* _tm;///<Current cfdTextureManager
    osg::ref_ptr<osg::Switch>_bboxSwitch;///<Bounding box switch node
    osg::ref_ptr<osg::Group> _visualBoundingBox;///<Geometry for the visual bounding box
    osg::ref_ptr<osg::Switch> _vvN;///<The top-level node of cfdVolumeVisualization
    osg::ref_ptr<osg::Group> _decoratorGroup;///<Decorator group for attaching appropriate shader state
    osg::ref_ptr<osg::Group> _byPassNode;///<By-pass node for setting appropriate shader state
    osg::ref_ptr<osg::TexGenNode> _texGenParams;///<Texture coordinate generation
    osg::BoundingBox _bbox;///<Bounding box
    osg::Vec3f _center;///<Bounding box center
    float _scale[3];///<Scale of the texture

    std::string _activeShader;///<The active shader
    std::map<std::string, ves::xplorer::volume::cfdOSGShaderManager*> _shaderManagers;///<The shaders.

};
} //end volume
} //end xplorer
} //end ves

#endif// CFD_VOLUME_VIZ_NODE_HANDLER_H
