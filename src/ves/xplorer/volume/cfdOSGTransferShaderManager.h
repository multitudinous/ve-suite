/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_OSG_TRANSFER_SHADER_MANAGER_H
#define CFD_OSG_TRANSFER_SHADER_MANAGER_H
/*!\file cfdOSGTransferShaderManager.h
* cfdOSGTransferShaderManager API
*/

/*!\class ves::xplorer::volume::cfdOSGTransferShaderManager
*
*/
#ifdef _OSG
namespace osg
{
class Texture3D;
class Texture1D;
class Texture2D;
class TexMat;
}
#include <vector>

namespace volume
    {}

#include <ves/xplorer/volume/cfdOSGShaderManager.h>
#include <ves/xplorer/volume/cfdUpdateableOSGTexture1d.h>

#include <string>
namespace ves
{
namespace xplorer
{
namespace volume
{
class cfdTextureManager;
class cfdUpdateTextureCallback;
class TransferFunction;
class PreIntegrationTexture2D;
class NoiseTexture2D;
class VE_TEXTURE_BASED_EXPORTS cfdOSGTransferShaderManager
            : public cfdOSGShaderManager
{
public:
    ///Constructor
    cfdOSGTransferShaderManager();
    ///Copy Constructor
    ///\param sm cfdOSGTransferShaderManager to copy
    cfdOSGTransferShaderManager( const cfdOSGTransferShaderManager& sm );
    ///Destructor
    virtual ~cfdOSGTransferShaderManager();
    ///Set the property texture by the current cfdTextureManager
    void SetUseTextureManagerForProperty( bool trueFalse );
    ///Initialize parameters
    virtual void Init();
    ///Set the texture dimensions
    ///\param x The texture dimesion in the s direction
    ///\param y The texture dimesion in the t direction
    ///\param z The texture dimesion in the r direction
    void SetFieldSize( unsigned int x, unsigned int y, unsigned int z );
    ///Update a transfer function
    ///\param type The transfer function type
    ///\param param New value
    ///\param whichFunction The index of the transfer function to update
    void UpdateTransferFunction( cfdUpdateableOSGTexture1d::TransType type,
                                 float param, int whichFunction );
    ///Set the property texture
    ///\param property The 3D property
    void SetPropertyTexture( osg::Texture3D* property );
    ///Set the texture matrix
    ///\param tmat The current texture matrix
    void SetTextureMatrix( osg::TexMat* tmat );
    ///Set the texture manager
    ///\param tm cfdTextureManager pointer
    void InitTextureManager( cfdTextureManager* tm );

    ///Update the cfdTextureManager
    ///\param tm cfdTextureManager pointer
    void UpdateTextureManager( cfdTextureManager* tm );

    ///Get the property texture
    osg::Texture3D* GetPropertyTexture();

    ///Equal operator
    ///\param sm cfdOSGTransferShaderManager to set this equal to
    virtual cfdOSGTransferShaderManager& operator=( const
                                                    cfdOSGTransferShaderManager& sm );
protected:
    ///Initialize the list of transfer functions
    virtual void _initTransferFunctions();
    ///Create and add a transfer function to the list
    ///\param gamma Use gamma correction
    ///\param clearList Clear the current list of transfer functions before adding a new one
    void _createTransferFunction( bool gamma = false,
                                  bool clearList = false );
    ///Initialize the property texture
    virtual void _initPropertyTexture();
    ///Set the osg::StateSet f
    void _setupStateSetForGLSL();
    unsigned int _fieldSize[3];///The texture dimension

    osg::ref_ptr<osg::TexMat> _texMat;///<The current texture matrix
    osg::ref_ptr<osg::Texture3D> _property;///<The property texture
    typedef osg::ref_ptr<osg::Texture1D> TransferFunction ;
    typedef osg::ref_ptr<osg::Texture2D> TransferFunction2D;
    std::vector<TransferFunction2D> _transferFunctions;///<The transfer functions
    bool _reinit;///<Reinit the parameters
    bool _useTM;///<Use the cfdTextureManager for texture data
    cfdTextureManager* _tm;///<cfdTextureManager
    osg::ref_ptr<cfdUpdateTextureCallback> _utCbk;///<The update callback for the texture
    ves::xplorer::volume::TransferFunction* _tf;///<The transfer function for preIntegration.
    ves::xplorer::volume::PreIntegrationTexture2D* _preIntTexture;///<The preIntegrated texture.
    ves::xplorer::volume::NoiseTexture2D* _jitterTexture;///<The noise texture
};
}
}
}
#endif//_OSG
#endif// CFD_OSG_SCALAR_SHADER_MANAGER_H
