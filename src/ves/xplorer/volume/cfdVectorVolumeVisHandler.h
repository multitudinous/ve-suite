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

#ifndef CFD_VECTOR_VOLUME_VIS_HANDLER_H
#define CFD_VECTOR_VOLUME_VIS_HANDLER_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>
#include <ves/xplorer/volume/cfdVolumeVisNodeHandler.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

namespace osg
{
class Group;
class Texture3D;
}

namespace ves
{
namespace xplorer
{
namespace volume
{
class cfdOSGAdvectionShaderManager;
class cfdOSGTransferShaderManager;
class cfdTextureManager;
class cfdPBufferManager;
class cfdUpdateTextureCallback;
class cfd3DTextureCullCallback;
class cfdOSGPingPongTexture3D;

/*!\file cfdVectorVolumeVisHandler.h
 * cfdVectorVolumeVisHandler API
 */

/*!\class ves::xplorer::volume::cfdVectorVolumeVisHandler
 *
 */
class VE_TEXTURE_BASED_EXPORTS cfdVectorVolumeVisHandler :
    public cfdVolumeVisNodeHandler
{
public:
    ///Constructor
    cfdVectorVolumeVisHandler();
    ///Copy Constructor
    ///\param vvnh cfdVectorVolumeVisHandler to copy
    cfdVectorVolumeVisHandler( const cfdVectorVolumeVisHandler& vvnh );
    ///Destructor
    virtual ~cfdVectorVolumeVisHandler();
    ///Initialize parameters
    virtual void Init();
    ///Set the cfdTextureManager
    ///\param tm cfdTextureManager pointer
    virtual void SetTextureManager( cfdTextureManager* tm );

    ///Set the cfdPBufferManager pointer
    ///\param pbm cfdPBufferManager pointer
    void SetPBufferManager( cfdPBufferManager* pbm );
    ///Ping-pong the advection textures
    void PingPongTextures();
    ///Set the current transient timestep
    ///\param whichTimeStep The index of the timestep in the cfdTextureManager
    ///\param makeSlave The Cluster information
    void SetCurrentTransientTexture( unsigned int whichTimeStep,
                                     bool makeSlave = false );

    ///Get the cfdOSGAdvectionShaderManager pointer
    cfdOSGAdvectionShaderManager* GetAdvectionShaderManager()
    {
        return _aSM;
    }
    ///Equal Operator
    ///\param vvnh cfdVectorVolumeVisHandler to set this equal to.
    cfdVectorVolumeVisHandler& operator=( const cfdVectorVolumeVisHandler& vvnh );

protected:
    ///Set up the decorator node
    virtual void _setUpDecorator();
    ///Apply the texture matrix
    virtual void _applyTextureMatrix();
    ///Create the texture "ping-ponger"
    void _createTexturePingPong();
    ///Iniit the property texturee
    void _initPropertyTexture();
    ///Create velocity field from current cfdTextureManager if it exists
    void _createVelocityFromTextureManager();
    ///Setup transfer shader
    void _createTransferShader();
    ///Setup transfer functions state sets
    void _setupTransferPropertyStateSet();
    ///Set up advection property texture
    void _setupAdvectionPropertyStateSet();

    bool _ssIsSet;///<StateSet is initializer

    cfdOSGAdvectionShaderManager* _aSM;///<Advection shader algorithm
    cfdOSGTransferShaderManager* _transferSM;///<Transfer funciton shader manager for advection algorithm

    osg::ref_ptr<cfd3DTextureCullCallback> _cullCallback;
    ;///<Cullcallback for advection algorithm
    osg::ref_ptr<cfdUpdateTextureCallback> _velocityCbk;///<Update callback for velocity texture
    cfdPBufferManager* _pbuffer;///<PBuffer
    cfdOSGPingPongTexture3D* _texturePingPong;///<Texture "Ping-pong"er

    osg::ref_ptr<osg::Group> _advectionSlice;///<Group for advection slice
    osg::ref_ptr<osg::Group> _propertyTextureGroup;///<Group of property texure
    osg::ref_ptr<osg::Texture3D> _property;///<Property texture
    osg::ref_ptr<osg::Texture3D> _velocity;///<Velocity texture

};
} //end volume
} //end xplorer
} //end ves

#endif// CFD_SCALAR_VOLUME_VIS_HANDLER_H
