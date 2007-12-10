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
#ifndef CFD_OSG_SHADER_MANAGER_H
#define CFD_OSG_SHADER_MANAGER_H
/*!\file cfdOSGShaderManager.h
* cfdOSGShaderManager API
*/

/*!\class ves::xplorer::volume::cfdOSGShaderManager
*
*/
#ifdef _OSG
#include <osg/StateSet>
namespace osg
{
class Shader;
}
#include <ves/VEConfig.h>
#include <string>
namespace ves
{
namespace xplorer
{
namespace volume
{
class VE_TEXTURE_BASED_EXPORTS cfdOSGShaderManager
{
public:
    ///Construtor
    cfdOSGShaderManager();
    ///Copy Constructor
    ///\param sm cfdOSGShaderManager to copy
    cfdOSGShaderManager( const cfdOSGShaderManager& sm );
    ///Destructor
    virtual ~cfdOSGShaderManager();

    ///Initialize parameters
    virtual void Init() = 0;

    ///DEPRICATED: Set the shader directory
    ///\param dir The directory containg the shader to load
    void SetShaderDirectory( std::string dir );
    ///Set the boundary of the data
    ///\param bounds The bounds of the data
    void SetBounds( float* bounds );

    ///Add a shader program to the shader manager
    ///\param name The name of the program
    ///\param glslProgram The glsl program
    void AddShaderProgram( std::string name, osg::ref_ptr<osg::Program> glslProgram );

    ///Set the active shader
    ///\param name The name of the shader to activate
    void SetActiveShaderProgram( std::string name );

    ///Get the shader state set
    osg::StateSet* GetShaderStateSet();
    ///Get the texture unit for auto generating texture coordinates
    unsigned int GetAutoGenTextureUnit()
    {
        return _tUnit;
    }

protected:
    ///Equal operator
    ///\param sm The cfdOSGShaderManager to set this equal to
    virtual cfdOSGShaderManager& operator=( const cfdOSGShaderManager& sm );

    ///Set up the shader
    ///\param glslProgram The osg::Program
    ///\param pgName The name of the program
    ///\param override Flag that determines if this stateeset attribute should override
    virtual void _setupGLSLShaderProgram( osg::Program* glslProgram,
                                          const std::string pgName,
                                          bool override = false );
    ///Create a shader from a file
    ///\param filename The file containing the shader code
    ///\param isFrag Fragment or Vertex shader
    virtual osg::Shader* _createGLSLShaderFromFile( const std::string filename,
                                                    bool isFrag );


    ///Create a shader from a string
    ///\param inlineSource The string containing the shader code
    ///\param isFrag Fragment or Vertex shader
    virtual osg::Shader*  _createGLSLShaderFromInline( const std::string inlineSource,
                                                       bool isFrag );

    ///Set up the shader and the osg::StateSet
    virtual void _setupStateSetForGLSL() = 0;

    osg::ref_ptr<osg::Shader> _vshader;///<Vertex shader
    osg::ref_ptr<osg::Shader> _fshader;///<Fragment shader

    std::map<std::string, osg::ref_ptr<osg::Program> > _programs;///<The available shader programs;
    osg::ref_ptr<osg::StateSet> _ss;///<osg::StateSet
    std::string _shaderDirectory;///<Directory containg the shaders
    unsigned int _tUnit;///<The texture unit
    float* _bounds;///<The data boundary
    bool _useGLSL;///<Use GLSL instead of CG
};
}
}
}
#endif //_OSG
#endif// CFD_OSG_SHADER_MANAGER_H
