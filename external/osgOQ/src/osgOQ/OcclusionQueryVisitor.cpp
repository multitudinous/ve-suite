//
// Copyright (C) 2007 Skew Matrix Software LLC (http://www.skew-matrix.com)
//
// This library is open source and may be redistributed and/or modified under  
// the terms of the OpenSceneGraph Public License (OSGPL) version 0.0 or 
// (at your option) any later version.  The full license is in LICENSE file
// included with this distribution, and on the openscenegraph.org website.
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
// OpenSceneGraph Public License for more details.
//

#include "osgOQ/OcclusionQueryVisitor.h"
#include "osgOQ/OcclusionQueryContext.h"
#include "osgOQ/OcclusionQueryRoot.h"
#include "osgOQ/OcclusionQueryNode.h"
#include <osgUtil/RenderBin>
#include <osg/BlendFunc>
#include <osgDB/FileUtils>
#include <osg/Notify>
#include <string>


using namespace osgOQ;


class VertexCounter : public osg::NodeVisitor
{
public:
    VertexCounter( int limit )
      : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN ),
        _limit( limit ),
        _total( 0 ) {}
    ~VertexCounter() {}

    int getTotal() { return _total; }
    bool exceeded() const { return _total > _limit; }
    void reset() { _total = 0; }

    virtual void apply( osg::Node& node )
    {
        // Check for early abort. If out total already exceeds the
        //   max number of vertices, no need to traverse further.
        if (exceeded())
            return;
        traverse( node );
    }

    virtual void apply( osg::Geode& geode )
    {
        // Possible early abort.
        if (exceeded())
            return;

        unsigned int i;
        for( i = 0; i < geode.getNumDrawables(); i++ )
        {
            osg::Geometry* geom = dynamic_cast<osg::Geometry *>(geode.getDrawable(i));
            if( !geom )
                continue;

            osg::Vec3Array* varray = dynamic_cast<osg::Vec3Array *>(geom->getVertexArray());
            if( varray == NULL )
                continue;

            _total += varray->size();
            if (_total > _limit)
                break;
        } 
    }

protected:
    int _limit;
    int _total;
};



OcclusionQueryNonFlatVisitor::OcclusionQueryNonFlatVisitor( OcclusionQueryContext* oqc )
  : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN ),
    _oqc( oqc )
{
    if (oqc == NULL)
        _oqc = new OcclusionQueryContext;
}

OcclusionQueryNonFlatVisitor::~OcclusionQueryNonFlatVisitor()
{
	osg::notify( osg::NOTICE ) <<
		"OcclusionQueryNonFlatVisitor: Added " << _oqc->getNameIdx() <<
		" OQNodes." << std::endl;
}

void
OcclusionQueryNonFlatVisitor::apply( osg::Node& node )
{
	if ( !(getOQContext()->nameAvailable()) )
	{
		// Can't create any more OQNs.
		osg::notify( osg::WARN ) << "OcclusionQueryVisitor: Exhausted OQNs." << std::endl;
		return;
	}

    if (node.getNumParents() == 0)
    {
        // Can't add an OQN above a root node.
        traverse( node );
        return;
    }

    osg::Group* grp = dynamic_cast<osg::Group*>( &node );
	if (grp != NULL)
	{
		osgOQ::OcclusionQueryRoot* thisOQR = dynamic_cast<osgOQ::OcclusionQueryRoot*>( &node );
		osgOQ::OcclusionQueryNode* thisOQN = dynamic_cast<osgOQ::OcclusionQueryNode*>( &node );
		if ( (thisOQR != NULL) || (thisOQN != NULL) )
			// A subgraph is already under osgOQ contr9ol.
			// Don't traverse further.
			return;

		if (grp->getNumChildren() <= 1)
		{
			// No point in adding an OQN above a Group node
			//   with only one child. Traverse, and add an
			//   OQN closer to the leaves. This avoids redundant
			//   OQNs that check the same bounding geometry.
	        traverse( node );
			return;
		}
	}

    VertexCounter vc( _oqc->getOccluderThreshold() );
    node.accept( vc );
    if (vc.exceeded())
    {
        // Insert OQN(s) above this node.
		unsigned int np = node.getNumParents();
		while (np--)
		{
			osg::Group* parent = dynamic_cast<osg::Group*>( node.getParent( np ) );
			if (parent != NULL)
			{
				osg::ref_ptr<OcclusionQueryNode> oqn = new OcclusionQueryNode( getOQContext() );
				oqn->addChild( &node );
				parent->replaceChild( &node, oqn.get() );
			}
		}

        traverse( node );
    }
    // Else,
    //   No need to traverse further if we didn't exceed the
    //   occluder threshold.
}




OcclusionQueryFlatVisitor::OcclusionQueryFlatVisitor( OcclusionQueryContext* oqc )
  : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN ),
    _oqc( oqc )
{
    if (oqc == NULL)
        _oqc = new OcclusionQueryContext;
}

OcclusionQueryFlatVisitor::~OcclusionQueryFlatVisitor()
{
	osg::notify( osg::NOTICE ) <<
		"OcclusionQueryFlatVisitor: Added " << _oqc->getNameIdx() <<
		" OQNodes." << std::endl;
}

void
OcclusionQueryFlatVisitor::apply( osg::Group& group )
{
	if ( !(getOQContext()->nameAvailable()) )
	{
		// Can't create any more OQNs.
		osg::notify( osg::WARN ) << "OcclusionQueryVisitor: Exhausted OQNs." << std::endl;
		return;
	}

	osgOQ::OcclusionQueryRoot* thisOQR = dynamic_cast<osgOQ::OcclusionQueryRoot*>( &group );
	osgOQ::OcclusionQueryNode* thisOQN = dynamic_cast<osgOQ::OcclusionQueryNode*>( &group );
	if ( (thisOQR != NULL) || (thisOQN != NULL) )
		// A subgraph is already under osgOQ control.
		// Don't traverse further.
		return;

    if (group.getNumParents() == 0)
    {
        // Can't add an OQN above a root node.
        traverse( group );
        return;
    }

	int preTraverseOQNCount = _oqc->getNameIdx();
	traverse( group );

	if (_oqc->getNameIdx() > preTraverseOQNCount)
		// A least one OQN was added below the current node.
		//   Don't add one here to avoid hierarchical nesting.
		return;
	
	// There are no OQNs below this group. If the vertex
	//   count exceeds the threshold, add an OQN here.
	addOQN( group );
}

void
OcclusionQueryFlatVisitor::apply( osg::Geode& geode )
{
	if ( !(getOQContext()->nameAvailable()) )
	{
		// Can't create any more OQNs.
		osg::notify( osg::WARN ) << "OcclusionQueryVisitor: Exhausted OQNs." << std::endl;
		return;
	}

    if (geode.getNumParents() == 0)
    {
        // Can't add an OQN above a root node.
        traverse( geode );
        return;
    }

	addOQN( geode );
}

void
OcclusionQueryFlatVisitor::addOQN( osg::Node& node )
{
    VertexCounter vc( _oqc->getOccluderThreshold() );
    node.accept( vc );
    if (vc.exceeded())
    {
        // Insert OQN(s) above this node.
		unsigned int np = node.getNumParents();
		while (np--)
		{
			osg::Group* parent = dynamic_cast<osg::Group*>( node.getParent( np ) );
			if (parent != NULL)
			{
				osg::ref_ptr<OcclusionQueryNode> oqn = new OcclusionQueryNode( getOQContext() );
				oqn->addChild( &node );
				parent->replaceChild( &node, oqn.get() );
			}
		}
    }
}
