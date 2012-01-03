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
//
// Copyright (c) 2009 Skew Matrix  Software LLC.
// All rights reserved.
//

#ifndef __WALL_H__
#define __WALL_H__ 1


#include <osg/Node>
#include <osg/Geode>
#include <osg/Geometry>

#include <iostream>
#include <vector>
#include <string>



class Wall
{
public:
    Wall( std::istream& in );
    void dump( std::ostream& out );

    void createGeode( const osg::Matrix& m );
    osg::Geode* getGeode() const;
    osg::Matrix getProj() const;
    osg::Matrix getView() const;

    std::string suffix_;
    osg::Vec3 corner_;
    osg::Vec3 w_, h_;
    std::string texName_;

    osg::ref_ptr< osg::Geode > geode_;
};
typedef std::vector< Wall > WallList;

WallList readWallFile( const std::string& fName, osg::Vec3& viewPos, std::string& prefix );


#endif
