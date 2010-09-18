/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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

#define VES_USE_FBO_CAMERA 0

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- OSG Includes --- //
#include <osg/Group>
#include <osg/Drawable>
#include <osgUtil/CullVisitor>

namespace osg
{
class Geode;
class Texture;
class Viewport;
}

// --- C/C++ Includes --- //
#include <map>
#include <vector>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace rtt
{
class VE_SCENEGRAPH_EXPORTS Unit : public osg::Group
{
public:
    ///
    //typedef std::map< unsigned int, osg::ref_ptr< osg::Texture > > TextureMap;
    //typedef std::map< osg::ref_ptr< Unit >,
    //    std::pair< std::string, unsigned int > > InputToUniformMap;

    typedef std::vector< osg::ref_ptr< osg::Texture > > TextureMap;
    typedef std::vector< std::pair< std::string, osg::ref_ptr< Unit > > > InputToUniformMap;

    ///Constructor
    Unit();

    ///Copy Constructor
    Unit(
        const Unit& unit,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    META_Node( rtt, Unit );

    ///Set an input from the given unit to be linked with a given uniform name
    ///\param unit Pointer to the unit which output to use
    ///\param uniform Name of the uniform to use to bind the texture to
    ///\param add If true will add the given parent to the parent list
    ///\return If uniform was successfully added
    bool SetInputToUniform(
        Unit* unit, const std::string& uniform, bool add = false );

    ///Remove an assigned parent output uniform
    ///\param parent Pointer to the parent node
    ///\param remove Should this unit be removed from parent
    ///Removed this function because we are using vectors for storage
    ///This function is not used at this time in any event
    //void RemoveInputToUniform( Unit* parent, bool remove = false );

    ///Get the map which maps uniform to input units
    ///\return
    const InputToUniformMap& GetInputToUniformMap() const;

    ///Return an input texture of a certain mrt index
    ///\param inputIndex Index of the input texture
    ///\return
    osg::Texture* const GetInputTexture( int inputIndex ) const;

    ///Return complete index to texture mapping
    ///\return
    const TextureMap& GetInputTextureMap() const;

    ///Return an output texture of a certain mrt index
    ///\param mrt
    ///\return
    osg::Texture* const GetOutputTexture( int mrt = 0 ) const;

    ///Return mOutputTextures
    ///\return
    const TextureMap& GetOutputTextureMap() const;

    ///Initialze the unit
    virtual void Initialize();

    ///Update the unit
    virtual void Update();

    ///Set viewport which is used for this Unit while rendering
    ///\param
    void SetViewport( osg::Viewport* viewport );

    ///Set the input texture to use as a reference for the viewport size
    ///\param index
    void SetInputTextureIndexForViewportReference( int index );

    ///Get the input texture with dimenstions used for setting up the viewport
    ///\return
    const int GetInputTextureIndexForViewportReference() const;

    ///Get geode to which the unit's drawables are attached
    ///\return
    osg::Geode* const GetGeode() const;

    ///Helper function to create viewport sized quads
    void CreateTexturedQuadDrawable(
        osg::Vec3 const& corner = osg::Vec3( -1.0, -1.0, -1.0 ),
        osg::Vec3 const& widthVec = osg::Vec3( 2.0, 0.0, 0.0 ),
        osg::Vec3 const& heightVec = osg::Vec3( 0.0, 2.0, 0.0 ),
        float l = 0.0, float b = 0.0, float r = 1.0, float t = 1.0 );

protected:
    ///Destructor
    virtual ~Unit();

    ///Use this method in derived classes to implement unit specific uniforms
    virtual void UpdateUniforms();

    ///Set the input textures based on the parents
    ///This does NOT scan parents for any uniforms. If a uniform is needed for 
    ///this unit it must explicitly be configured through the SetInputToUniform
    ///interface. 
    ///This function should really be a pure virtual because every unit is going
    ///to have a different implementation. This default implementation
    ///in specifically for UnitInOut's.
    virtual void SetInputTexturesFromParents();

    ///Notice underlying classes, that viewport size is changed
    virtual void NoticeChangeViewport();

    ///Assign the input texture to the quad object
    void AssignInputTexture();

    ///Is the unit active, yes/no
    bool mActive;

    ///Index of the input texture which size is used as viewport
    int mInputTextureIndexForViewportReference;

    ///Input textures
    TextureMap mInputTextures;

    ///Output textures
    TextureMap mOutputTextures;

    ///Map of the uniform to parent links
    InputToUniformMap mInputToUniformMap;

    ///Geode used to setup the unit's drawable
    osg::ref_ptr< osg::Geode > mGeode;

    ///Store a screen sized quad so it can be used for rendering
    osg::ref_ptr< osg::Drawable > mDrawable;

    ///Store the viewport of the camera
    osg::ref_ptr< osg::Viewport > mViewport;

private:

};
} //end rtt
} //end scenegraph
} //end xplorer
} //end ves

#endif //UNIT_H
