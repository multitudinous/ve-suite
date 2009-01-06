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

#ifndef UNIT_H
#define UNIT_H

// --- OSG Includes --- //
#include <osg/Group>

namespace osg
{
class Geode;
class Texture;
class Shader;
class Drawable;
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
    typedef std::map< unsigned int, osg::ref_ptr< osg::Texture > > TextureMap;
    
    ///Constructor
    Unit();

    ///Copy Constructor
    Unit(
        const Unit& unit,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    META_Node( rtt, Unit );


    ///Return an input texture of a certain index
    ///\param index Index of the input texture (index is equal to the texture unit)
    osg::Texture* const GetInputTexture( int inputIndex ) const;

protected:
    ///Destructor
    virtual ~Unit();

    ///Is the unit active, yes/no
    bool mActive;

    ///Input textures
    TextureMap mInputTextures;

    ///Output textures
    TextureMap mOutputTextures;

    ///Geode used to setup the unit's drawable
    osg::ref_ptr< osg::Geode > mGeode;

    ///Shader which will be used for rendering
    osg::ref_ptr< osg::Shader > mShader;

    ///Store a screen sized quad so it can be used for rendering
    osg::ref_ptr< osg::Drawable > mDrawable;

    ///Store the viewport of the camera
    osg::ref_ptr< osg::Viewport > mViewport;

private:

};
} //end rtt
} //end xplorer
} //end ves

#endif //UNIT_H
