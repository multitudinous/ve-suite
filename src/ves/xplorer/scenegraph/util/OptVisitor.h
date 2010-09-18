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
//
// Copyright (c) 2009 Skew Matrix  Software LLC.
// All rights reserved.
//

#ifndef __OPT_VISITOR_H__
#define __OPT_VISITOR_H__


#include <osg/NodeVisitor>
#include <osg/Geometry>
#include <iostream>


class OptVisitor : public osg::NodeVisitor
{
public:
    OptVisitor();
    virtual ~OptVisitor();

    //META_NodeVisitor(osgBullet,OptVisitor)

    virtual void apply( osg::Node& node );
    virtual void apply( osg::Geode& geode );

    bool changeDLtoVBO_;
    bool changeDynamicToStatic_;
    bool changeDAtoDEUI_;

    void dump( std::ostream& ostr );

protected:
    void processTriangles( const osg::DrawArrays& da, osg::VectorGLuint& indices );
    void processTriFan( const osg::DrawArrays& da, osg::VectorGLuint& indices );
    void processTriStrip( const osg::DrawArrays& da, osg::VectorGLuint& indices );

    unsigned int triangles_;
    unsigned int triFans_;
    unsigned int triStrips_;
    unsigned int newDEUIs_;
};


#endif
