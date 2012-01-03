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
#ifndef __COUNTS_VISITOR_H__
#define __COUNTS_VISITOR_H__

#include <osg/NodeVisitor>
#include <set>

class CountsVisitor : public osg::NodeVisitor
{
public:
    CountsVisitor( osg::NodeVisitor::TraversalMode mode = osg::NodeVisitor::TRAVERSE_ACTIVE_CHILDREN );
    ~CountsVisitor();

    void reset();

    void dump();

    void apply( osg::Node& node );
    void apply( osg::Group& node );
    void apply( osg::LOD& node );
    void apply( osg::PagedLOD& node );
    void apply( osg::Switch& node );
    void apply( osg::Sequence& node );
    void apply( osg::Transform& node );
    void apply( osg::MatrixTransform& node );
    void apply( osg::Geode& node );

    int getVertices() const;
    int getDrawArrays() const;

protected:
    int _depth;
    int _maxDepth;

    int _nodes;
    int _groups;
    int _lods;
    int _pagedLods;
    int _switches;
    int _sequences;
    int _transforms;
    int _matrixTransforms;
    int _dofTransforms;
    int _geodes;
    int _drawables;
    int _geometries;
    int _nullGeometries;
    int _texts;
    int _vertices;
    int _stateSets;
    int _textures;
    int _primitiveSets;
    int _drawArrays;

    int _totalChildren;
    int _slowPathGeometries;

    typedef std::set< osg::ref_ptr<osg::Object> > ObjectSet;
    ObjectSet _uNodes;
    ObjectSet _uGroups;
    ObjectSet _uLods;
    ObjectSet _uPagedLods;
    ObjectSet _uSwitches;
    ObjectSet _uSequences;
    ObjectSet _uTransforms;
    ObjectSet _uMatrixTransforms;
    ObjectSet _uDofTransforms;
    ObjectSet _uGeodes;
    ObjectSet _uDrawables;
    ObjectSet _uGeometries;
    ObjectSet _uTexts;
    ObjectSet _uVertices;
    ObjectSet _uStateSets;
    ObjectSet _uTextures;
    ObjectSet _uPrimitiveSets;
    ObjectSet _uDrawArrays;
};

#endif
