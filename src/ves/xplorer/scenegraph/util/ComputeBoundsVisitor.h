/*************** <auto-copyright.rb BEGIN do not edit this line> *************
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
 *************** <auto-copyright.rb END do not edit this line> **************/
#ifndef COMPUTE_BOUNDS_VISITOR_H
#define COMPUTE_BOUNDS_VISITOR_H

#include <ves/VEConfig.h>

//#include <osg/ref_ptr>
#include <osg/NodeVisitor>
#include <osg/BoundingBox>
#include <osg/Polytope>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace util
{
class VE_SCENEGRAPH_UTILS_EXPORTS ComputeBoundsVisitor : public osg::NodeVisitor
{
public:
    ComputeBoundsVisitor( TraversalMode traversalMode = TRAVERSE_ALL_CHILDREN );

    virtual void reset();

    osg::BoundingBox& getBoundingBox()
    {
        return _bb;
    }

    void getPolytope( osg::Polytope& polytope, float margin = 0.1 ) const;

    void getBase( osg::Polytope& polytope, float margin = 0.1 ) const;

    void apply( osg::Node& node );

    void apply( osg::Transform& transform );

    void apply( osg::Geode& geode );

    inline void pushMatrix( osg::Matrix& matrix )
    {
        _matrixStack.push_back( matrix );
    }

    inline void popMatrix()
    {
        _matrixStack.pop_back();
    }

    void applyDrawable( osg::Drawable* drawable );

protected:
    typedef std::vector< osg::Matrix > MatrixStack;

    MatrixStack _matrixStack;
    osg::BoundingBox _bb;
};
}
}
}
}

#endif //COMPUTE_BOUNDS_VISITOR_H
