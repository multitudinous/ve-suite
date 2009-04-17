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
    void RemoveInputToUniform( Unit* parent, bool remove = false );

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

    ///Helper function to create screen sized quads
    void CreateTexturedQuadDrawable(
        const osg::Vec3& corner = osg::Vec3( 0, 0, 0 ),
        const osg::Vec3& widthVec = osg::Vec3( 1, 0, 0 ),
        const osg::Vec3& heightVec = osg::Vec3( 0, 1, 0 ),
        float l = 0.0, float b = 0.0, float r = 1.0, float t = 1.0 );

protected:
    ///Destructor
    virtual ~Unit();

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


        ///This draw callback is used for customized drawing
    class CullCallback : public osg::Drawable::CullCallback
    {
    public:
        ///Default Constructor
        CullCallback();

        ///Copy Constructor
        CullCallback(
            const CullCallback& drawCallback,
            const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

            virtual bool cull(osg::NodeVisitor*, osg::Drawable*, osg::State*) const;
        ///
        META_Object( rtt, CullCallback );
            virtual bool cull(osg::NodeVisitor* nv, osg::Drawable* drawable, osg::RenderInfo* renderInfo) const;

    protected:
        ///Destructor
        virtual ~CullCallback();

    private:
    };

    ///Use this method in derived classes to implement unit specific uniforms
    virtual void UpdateUniforms();

    ///Set the input textures based on the parents
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

    ///Projection matrix of the ppu (default: 2D ortho view)
    osg::ref_ptr< osg::RefMatrix > mProjectionMatrix;

    ///Modelview matrix of the ppu (default: identity matrix)
    osg::ref_ptr< osg::RefMatrix > mModelViewMatrix;

private:

};
} //end rtt
} //end scenegraph
} //end xplorer
} //end ves

#endif //UNIT_H
