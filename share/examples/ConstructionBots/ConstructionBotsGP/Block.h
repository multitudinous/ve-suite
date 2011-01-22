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

#ifndef BLOCK_H
#define BLOCK_H

// --- OSG Includes --- //
#include <osg/Geode>

namespace osg
{
    class Geometry;
}

// --- C/C++ Libraries --- //
#include <map>

namespace bots
{
class Block : public osg::Geode
{
public:
    ///Default Constructor
    Block();

    ///Constructor
    Block( const Block& block,
           const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///Register node with OSG
    META_Node( bots, Block );

    ///Returns the color of this block geode's lines
    const osg::Vec4& GetLineColor() const;

    ///Returns the color of this block geode
    const osg::Vec4& GetBlockColor() const;

    ///Return the requested drawable of this block geode
    const osg::Geometry* GetDrawable( unsigned int position ) const;

    ///Set the color of a drawable for this block geode
    void SetColor( unsigned int drawable, const osg::Vec4& color );

protected:
    ///Destructor
    virtual ~Block();

private:
    ///Initialize this block geode
    void Initialize();

    ///The color of this block geode's lines
    osg::Vec4 mLineColor;

    ///The color of this block geode
    osg::Vec4 mBlockColor;

    ///The drawables of this block geode
    /*  Lines   Sides
          1       5
        2 B 0   6 B 4
          3       7
    */
    std::map< unsigned int, osg::ref_ptr< osg::Geometry > > mDrawables;

};
} //end bots

#endif //BLOCK_H
