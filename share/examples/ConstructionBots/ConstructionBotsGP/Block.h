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
#include <string>

namespace bots
{
class Block : public osg::Geode
{
public:
    Block();

    Block( const Block& block, const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    META_Node( bots, Block );

    void CreateBlock();

    void SetColor( float r, float g, float b, float a );

protected:
    virtual ~Block();

private:
    void Initialize();

    std::map< std::string, osg::ref_ptr< osg::Geometry > > mSideStates;

};
} //end bots

#endif //BLOCK_H
