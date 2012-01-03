/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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

#ifndef GRID_H
#define GRID_H

// --- OSG Libraries --- //
#include <osg/Geode>

// --- C/C++ Libraries --- //
#include <vector>

namespace bots
{
class Grid : public osg::Geode
{
public:
    ///Default Constructor
    Grid();

    ///Constructor
    Grid( const Grid& grid,
          const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///Register node with OSG
    META_Node( bots, Grid );

    ///Create this grid geode
    void CreateGrid(
        int gridSize, std::map< std::pair< int, int >,
                                std::pair< bool, bool > >& occupancyMatrix );

protected:
    ///Destructor
    virtual ~Grid();

private:
    ///Initialize this grid geode
    void Initialize();

};
} //end bots

#endif //GRID_H
