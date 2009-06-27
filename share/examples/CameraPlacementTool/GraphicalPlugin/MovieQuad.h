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

#ifndef MOVIE_QUAD_H
#define MOVIE_QUAD_H

// --- VE-Suite Includes --- //
namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class DCS;
class CADEntity;
}
}
}

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/Vec3>

namespace osg
{
class Geometry;
class Geode;
class Image;
class ImageStream;
}

// --- C/C++ Libraries --- //
#include <string>

namespace cpt
{
class MovieQuad
{
public:
    MovieQuad( ves::xplorer::scenegraph::DCS* parentDCS );

    virtual ~MovieQuad();

protected:

private:
    osg::Geometry* CreateTexturedQuadGeometry(
        const osg::Vec3& pos, float width, float height, osg::Image* image,
        bool useTextureRectangle, bool xzPlane, bool optionFlip );

    void Initialize();

    void SetNameAndDescriptions( const std::string& name );

    osg::ref_ptr< osg::Geode > mGeode;

    osg::ref_ptr< osg::ImageStream > mImageStream;

    ves::xplorer::scenegraph::CADEntity* mCADEntity;

};
} //end cpt

#endif //end MOVIE_QUAD_H
