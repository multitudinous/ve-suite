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

#ifndef UNIT_H
#define UNIT_H

// --- OSG Includes --- //
#include <osg/Group>
#include <osg/Drawable>

namespace osg
{
class Geode;
class Texture;
class Shader;
class Viewport;
}

// --- C/C++ Includes --- //
#include <map>

namespace ves
{
namespace xplorer
{
namespace rtt
{
class Unit : public osg::Group
{
public:
    ///
    typedef std::vector< unsigned int > IgnoreInputList;
    typedef std::map< unsigned int, osg::ref_ptr< osg::Texture > > TextureMap;
    typedef std::map< osg::ref_ptr< Unit >,
        std::pair< std::string, unsigned int > > InputToUniformMap;
    
    ///Constructor
    Unit();

    ///Copy Constructor
    Unit(
        const Unit& unit,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    META_Node( rtt, Unit );

    ///Initialze the unit
    virtual void Initialize();

    ///Return an input texture of a certain mrt index
    ///\param index Index of the input texture (index is equal to the texture unit)
    osg::Texture* const GetInputTexture( int inputIndex ) const;

    ///Return an output texture of a certain mrt index
    ///NOTE: If you haven't initialized the Unit before calling this method
    ///it might end up in a NULL as output texture. For this purpose do use
    ///the getOrCreateOutputTexture()
    ///\param mrt
    osg::Texture* const GetOutputTexture( int mrt = 0 ) const;

    ///Return mOutputTextures
    const Unit::TextureMap& GetOutputTextureMap() const;

    ///Remove an assigned parent output uniform. @see assignParentToUniform()
    ///\param uniform Name of the uniform
    ///\param del Should this unit be removed from the child list of the parent connected with the given uniform [default=false]
    void RemoveInputToUniform( const std::string& uniform, bool remove = false );

    /// Remove an assigned parent output uniform. @see assignParentToUniform()
    ///\param parent Pointer to the parent node
    ///\param del Should this unit be removed from the child list of this parent [default=false]
    void RemoveInputToUniform( Unit* parent, bool remove = false );

    /**
    * Set an input from the given parent to be linked with the given
    * uniform name. This is required to automatically setup uniforms for
    * input textures of the assigned shader, which is based on the index
    * of the given parent unit in the parent list.
    * The type of the given uniform will be equivalent to the type of the
    * input texture (e.g. SAMPLER_2D = Texture2D).
    * @param parent Pointer to the parent which output to use
    * @param uniform Name of the uniform to use to bind the texture to
    * @param add if true will add the given parent to the parent list
    *             (same as calling parent->addChild()) [default=false]
    * @return true if uniform is set or false otherwise
    **/
    bool SetInputToUniform(
        Unit* parent, const std::string& uniform, bool add = false );

protected:
    ///Destructor
    virtual ~Unit();

    ///Set the input textures based on the parents
    virtual void SetInputTexturesFromParents();

    ///This draw callback is used for customized drawing
    class DrawCallback : public osg::Drawable::DrawCallback
    {
    public:
        ///Default Constructor
        DrawCallback();

        ///Constructor
        DrawCallback( Unit* parent );

        ///Copy Constructor
        DrawCallback(
            const DrawCallback& drawCallback,
            const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

        ///
        META_Object( rtt, DrawCallback );

        ///Do customized draw code
        virtual void drawImplementation(
            osg::RenderInfo& ri, const osg::Drawable* dr ) const;

    protected:
        ///Destructor
        ~DrawCallback();

    private:
        ///
        osg::ref_ptr< Unit > mParent;
    };

    ///Is the unit active, yes/no
    bool mActive;

    ///Input textures
    TextureMap mInputTextures;

    ///Output textures
    TextureMap mOutputTextures;

    ///Map of the uniform to parent links
    InputToUniformMap mInputToUniformMap;

    ///Geode used to setup the unit's drawable
    osg::ref_ptr< osg::Geode > mGeode;

    ///Shader which will be used for rendering
    osg::ref_ptr< osg::Shader > mShader;

    ///Store a screen sized quad so it can be used for rendering
    osg::ref_ptr< osg::Drawable > mDrawable;

    ///Store the viewport of the camera
    osg::ref_ptr< osg::Viewport > mViewport;

    ///Projection matrix of the ppu (default: 2D ortho view)
    osg::ref_ptr< osg::RefMatrix > mProjectionMatrix;

    ///Modelview matrix of the ppu (default: identity matrix)
    osg::ref_ptr< osg::RefMatrix > mModelViewMatrix;

private:

};
} //end rtt
} //end xplorer
} //end ves

#endif //UNIT_H
