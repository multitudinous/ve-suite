/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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

#pragma once
#include <osg/Texture2D>
#include <vector>

namespace ves
{
namespace conductor
{
class TextureSubloader : public osg::Texture2D::SubloadCallback
{
public:
    ///Constructor
    TextureSubloader();
    ///Destructor
    virtual ~TextureSubloader();

    ///create the OpenGL texture. A necessary override of osg::Texture2D::SubloadCallback (overrides a pure virtual).
    virtual void load( const osg::Texture2D& texture, osg::State& state ) const;


    ///overlay the image onto the texture. A necessary override of osg::Texture2D::SubloadCallback (overrides a pure virtual).
    virtual void subload( const osg::Texture2D& texture, osg::State& state ) const;

    void SetEnabled( bool enabled );

    ///tell the next subload callback to copy the input image to the specified offsets
    ///in the texture for this.
    void AddUpdate( osg::Image* img, int xOff, int yOff );

    ///Clear the current vectors of data
    void ClearData();

protected:
    mutable std::vector< int > xOffsets;                              // the X offset for the next subload operation.
    mutable std::vector< int > yOffsets;                              // the Y offset for the next subload operation.
    mutable bool doSubload;                   // true if should do a subload copy with next subload() callback for the texture.
    mutable std::vector< osg::ref_ptr< osg::Image > > subImgs;  // a pointer to an image to overlay onto the texture for this.

    typedef std::set< unsigned int > ContextSet;
    mutable ContextSet _contexts;
    bool m_enabled;
};

}
}
