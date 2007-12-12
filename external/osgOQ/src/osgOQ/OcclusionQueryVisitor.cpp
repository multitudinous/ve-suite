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
#include "osgOQ/OcclusionQueryNode.h"
#include "osgOQ/QueryState.h"
#include "osgOQ/OptionLoader.h"
#include <osgUtil/RenderBin>
#include <osg/StateAttribute>
#include <osg/PolygonMode>
#include <osg/ColorMask>
#include <osg/PolygonOffset>
#include <osg/Depth>
#include <osgDB/FileUtils>
#include <osg/Notify>
#include <sstream>
#include <string>


using namespace osgOQ;


unsigned int countGeometryVertices( osg::Geometry* geom )
{
    if (!geom->getVertexArray())
        return 0;

    // TBD This will eventually iterate over the PrimitiveSets and total the
    //   number of vertices actually used. But for now, it just returns the
    //   size of the vertex array.

    return geom->getVertexArray()->getNumElements();
}

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

            _total += countGeometryVertices( geom );

            if (_total > _limit)
                break;
        } 
    }

protected:
    int _limit;
    int _total;
};



OcclusionQueryNonFlatVisitor::OcclusionQueryNonFlatVisitor()
  : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN ),
    _nameIdx( 0 ),
    _occluderThreshold( 5000 )
{
    int clampMe;
    if ( OptionLoader::instance()->getOption( "OccluderThreshold", clampMe ) )
        _occluderThreshold = (clampMe < 0 ) ? 0 : static_cast<unsigned int>( clampMe );

    // Init StateSet for occlusion query geometry.
    _state = initOQState();
    // Initialize StateSet for debug geometry
    _debugState = initOQDebugState();
}

OcclusionQueryNonFlatVisitor::~OcclusionQueryNonFlatVisitor()
{
    osg::notify( osg::INFO ) <<
        "osgOQ: OcclusionQueryNonFlatVisitor: Added " << getNameIdx() <<
        " OQNodes." << std::endl;
}

void
OcclusionQueryNonFlatVisitor::setOccluderThreshold( int vertices )
{
    _occluderThreshold = vertices;
}
int
OcclusionQueryNonFlatVisitor::getOccluderThreshold() const
{
    return _occluderThreshold;
}

void
OcclusionQueryNonFlatVisitor::apply( osg::Node& node )
{
    if (node.getNumParents() == 0)
    {
        // Can't add an OQN above a root node.
        traverse( node );
        return;
    }

    osg::Group* grp = dynamic_cast<osg::Group*>( &node );
    if (grp != NULL)
    {
        osgOQ::OcclusionQueryNode* thisOQN = dynamic_cast<osgOQ::OcclusionQueryNode*>( &node );
        if (thisOQN != NULL)
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

    VertexCounter vc( _occluderThreshold );
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
                osg::ref_ptr<OcclusionQueryNode> oqn = new OcclusionQueryNode();
                oqn->addChild( &node );
                parent->replaceChild( &node, oqn.get() );

                oqn->setName( getNextOQNName() );
                // Set all OQNs to use the same query StateSets (instead of multiple copies
                //   of the same StateSet) for efficiency.
                oqn->setQueryStateSets( _state.get(), _debugState.get() );
            }
        }

        traverse( node );
    }
    // Else,
    //   No need to traverse further if we didn't exceed the
    //   occluder threshold.
}

std::string
OcclusionQueryNonFlatVisitor::getNextOQNName()
{
    std::ostringstream ostr;
    ostr << "OQNode_" << _nameIdx++;
    return ostr.str();
}



OcclusionQueryFlatVisitor::OcclusionQueryFlatVisitor()
  : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN ),
    _nameIdx( 0 ),
    _occluderThreshold( 5000 )
{
    int clampMe;
    if ( OptionLoader::instance()->getOption( "OccluderThreshold", clampMe ) )
        _occluderThreshold = (clampMe < 0 ) ? 0 : static_cast<unsigned int>( clampMe );

    // Init StateSet for occlusion query geometry.
    _state = initOQState();
    // Initialize StateSet for debug geometry
    _debugState = initOQDebugState();
}

OcclusionQueryFlatVisitor::~OcclusionQueryFlatVisitor()
{
    osg::notify( osg::INFO ) <<
        "osgOQ: OcclusionQueryFlatVisitor: Added " << getNameIdx() <<
        " OQNodes." << std::endl;
}

void
OcclusionQueryFlatVisitor::setOccluderThreshold( int vertices )
{
    _occluderThreshold = vertices;
}
int
OcclusionQueryFlatVisitor::getOccluderThreshold() const
{
    return _occluderThreshold;
}

void
OcclusionQueryFlatVisitor::apply( osg::Group& group )
{
    osgOQ::OcclusionQueryNode* thisOQN = dynamic_cast<osgOQ::OcclusionQueryNode*>( &group );
    if (thisOQN != NULL)
        // A subgraph is already under osgOQ control.
        // Don't traverse further.
        return;

    if (group.getNumParents() == 0)
    {
        // Can't add an OQN above a root node.
        traverse( group );
        return;
    }

    int preTraverseOQNCount = getNameIdx();
    traverse( group );

    if (getNameIdx() > preTraverseOQNCount)
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
    VertexCounter vc( _occluderThreshold );
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
                osg::ref_ptr<OcclusionQueryNode> oqn = new OcclusionQueryNode();
                oqn->addChild( &node );
                parent->replaceChild( &node, oqn.get() );

                oqn->setName( getNextOQNName() );
                // Set all OQNs to use the same query StateSets (instead of multiple copies
                //   of the same StateSet) for efficiency.
                oqn->setQueryStateSets( _state.get(), _debugState.get() );
            }
        }
    }
}

std::string
OcclusionQueryFlatVisitor::getNextOQNName()
{
    std::ostringstream ostr;
    ostr << "OQNode_" << _nameIdx++;
    return ostr.str();
}




//
PerDrawableQueryVisitor::PerDrawableQueryVisitor()
  : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN ),
    _occluderThreshold( 5000 ),
    _count( 0 )
{
    int clampMe;
    if ( OptionLoader::instance()->getOption( "OccluderThreshold", clampMe ) )
        _occluderThreshold = (clampMe < 0 ) ? 0 : static_cast<unsigned int>( clampMe );
}

PerDrawableQueryVisitor::~PerDrawableQueryVisitor()
{
    osg::notify( osg::INFO ) << "osgOQ: PerDrawableQueryVisitor: Moved " << _count << " Drawables." << std::endl;
}

void
PerDrawableQueryVisitor::setOccluderThreshold( int vertices )
{
    _occluderThreshold = vertices;
}
int
PerDrawableQueryVisitor::getOccluderThreshold() const
{
    return _occluderThreshold;
}

void
PerDrawableQueryVisitor::apply( osg::Group& group )
{
    traverse( (osg::Node&)group );

    GeodeList::const_iterator it = _newChildren.begin();
    while (it != _newChildren.end())
    {
        group.addChild( (*it).get() );
        it++;
    }

    _newChildren.clear();
}

void
PerDrawableQueryVisitor::apply( osg::Geode& geode )
{
    typedef std::vector< osg::ref_ptr<osg::Geode> > GeodeList;
    GeodeList gl;

    // Identify target Drawable(s), create Geodes for each,
    //   remove drawables from original Geode, and add new
    //   Geodes to a list for later processing.
    unsigned int nd = geode.getNumDrawables();
    while (nd-- > 0)
    {
        osg::Geometry* geom = dynamic_cast<osg::Geometry*>( geode.getDrawable( nd ) );
        if (!geom)
            continue;
        if (countGeometryVertices( geom ) >= _occluderThreshold)
        {
            osg::ref_ptr<osg::Geode> newGeode = copyGeodeNoChildren( &geode );
            newGeode->addDrawable( geom );
            _newChildren.push_back( newGeode );
            geode.removeDrawable( geom );

            _count++;
        }
    }
}

osg::Geode*
PerDrawableQueryVisitor::copyGeodeNoChildren( osg::Geode* src )
{
    osg::Geode* g = new osg::Geode();

    // Object
    g->setName( src->getName() );
    g->setDataVariance( src->getDataVariance() );
    g->setUserData( src->getUserData() );

    // Node
    g->setInitialBound( src->getInitialBound() );
    g->setUpdateCallback( src->getUpdateCallback() );
    g->setCullCallback( src->getCullCallback() );
    g->setCullingActive( src->getCullingActive() );
    g->setNodeMask( src->getNodeMask() );
    g->setDescriptions( src->getDescriptions() );
    g->setStateSet( src->getStateSet() );

    return g;
}



void
VisibilityThresholdVisitor::apply( osg::Node& node )
{
    osgOQ::OcclusionQueryNode* oqn = dynamic_cast<osgOQ::OcclusionQueryNode*>( &node );
    if (oqn != NULL)
        oqn->setVisibilityThreshold( _visThreshold );
    traverse( node );
}

void
QueryFrameCountVisitor::apply( osg::Node& node )
{
    osgOQ::OcclusionQueryNode* oqn = dynamic_cast<osgOQ::OcclusionQueryNode*>( &node );
    if (oqn != NULL)
        oqn->setQueryFrameCount( _count );
    traverse( node );
}

void
EnableQueryVisitor::apply( osg::Node& node )
{
    osgOQ::OcclusionQueryNode* oqn = dynamic_cast<osgOQ::OcclusionQueryNode*>( &node );
    if (oqn != NULL)
        oqn->setQueriesEnabled( _enabled );
    traverse( node );
}


void 
DebugDisplayVisitor::apply( osg::Node& node )
{
    osgOQ::OcclusionQueryNode* oqn = dynamic_cast<osgOQ::OcclusionQueryNode*>( &node );
    if (oqn != NULL)
        oqn->setDebugDisplay( _debug );
    traverse( node );
}


RemoveOcclusionQueryVisitor::RemoveOcclusionQueryVisitor()
  : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN )
{
}

RemoveOcclusionQueryVisitor::~RemoveOcclusionQueryVisitor()
{
}

void
RemoveOcclusionQueryVisitor::apply( osg::Node& node )
{
    if (node.getNumParents() == 0)
    {
        // Even if this is an OQN, can't delete it because it's the root.
        traverse( node );
        return;
    }

    osgOQ::OcclusionQueryNode* oqnRaw = dynamic_cast<osgOQ::OcclusionQueryNode*>( &node );
    if (!oqnRaw)
    {
        traverse( node );
        return;
    }
    osg::ref_ptr<OcclusionQueryNode> oqn = oqnRaw;


    unsigned int np = oqn->getNumParents();
    while (np--)
    {
        osg::Group* parent = dynamic_cast<osg::Group*>( oqn->getParent( np ) );
        if (parent != NULL)
        {
            // Remove OQN from parent.
            parent->removeChild( oqn.get() );

            // Add OQN's children to parent.
            unsigned int nc = oqn->getNumChildren();
            while (nc--)
                parent->addChild( oqn->getChild( nc ) );
        }
    }
}



StatisticsVisitor::StatisticsVisitor( osg::NodeVisitor::TraversalMode mode )
  : osg::NodeVisitor( mode ),
    _numOQNs( 0 ),
    _numPassed( 0 )
{
}

StatisticsVisitor::~StatisticsVisitor()
{
}

void
StatisticsVisitor::apply( osg::Node& node )
{
    osgOQ::OcclusionQueryNode* oqn = dynamic_cast<osgOQ::OcclusionQueryNode*>( &node );
    if (oqn != NULL)
    {
        _numOQNs++;
        if (oqn->getPassed())
            _numPassed++;
    }

    traverse( node );
}

void
StatisticsVisitor::reset()
{
    _numOQNs = _numPassed = 0;
}

unsigned int
StatisticsVisitor::getNumOQNs() const
{
    return _numOQNs;
}
unsigned int
StatisticsVisitor::getNumPassed() const
{
    return _numPassed;
}

