/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
#include <osg/Version>
#if ((OSG_VERSION_MAJOR>=2) && (OSG_VERSION_MINOR>=4))

#include <ves/xplorer/scenegraph/util/OcclusionQueryVisitor.h>
#include <osg/OcclusionQueryNode>
//#include "osgOQ/QueryState.h"
//#include "osgOQ/OptionLoader.h"
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
using namespace osg;


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
    //if ( OptionLoader::instance()->getOption( "OccluderThreshold", clampMe ) )
    //    _occluderThreshold = (clampMe < 0 ) ? 0 : static_cast<unsigned int>( clampMe );
    osg::ref_ptr<osg::OcclusionQueryNode> oqn = new osg::OcclusionQueryNode;

    // Init StateSet for occlusion query geometry.
    _state = oqn->getQueryStateSet();
    // Initialize StateSet for debug geometry
    _debugState = oqn->getDebugStateSet();
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
        osg::OcclusionQueryNode* thisOQN = dynamic_cast<osg::OcclusionQueryNode*>( &node );
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
                oqn->setQueryStateSet( _state.get() );
                oqn->setDebugStateSet( _debugState.get() );
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
    //if ( OptionLoader::instance()->getOption( "OccluderThreshold", clampMe ) )
    //    _occluderThreshold = (clampMe < 0 ) ? 0 : static_cast<unsigned int>( clampMe );
    osg::ref_ptr<osg::OcclusionQueryNode> oqn = new osg::OcclusionQueryNode;
    
    // Init StateSet for occlusion query geometry.
    _state = oqn->getQueryStateSet();
    // Initialize StateSet for debug geometry
    _debugState = oqn->getDebugStateSet();
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
    osg::OcclusionQueryNode* thisOQN = dynamic_cast<osg::OcclusionQueryNode*>( &group );
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
                oqn->setQueryStateSet( _state.get() );
                oqn->setDebugStateSet( _debugState.get() );
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
    //if ( OptionLoader::instance()->getOption( "OccluderThreshold", clampMe ) )
    //    _occluderThreshold = (clampMe < 0 ) ? 0 : static_cast<unsigned int>( clampMe );
}

PerDrawableQueryVisitor::~PerDrawableQueryVisitor()
{
    if( !_newGroupChildren.empty() || !_newLODChildren.empty() )
        rearrange();
}

int
PerDrawableQueryVisitor::rearrange()
{
    unsigned int count = 0;

    GroupMap::const_iterator it = _newGroupChildren.begin();
    while (it != _newGroupChildren.end())
    {
        osg::Group* grp = (*it).first.get();

        const GeodeList& gList = (*it).second;
        GeodeList::const_iterator git = gList.begin();
        while( git != gList.end())
        {
            grp->addChild( (*git).get() );
            count++;
            git++;
        }

        it++;
    }
    _newGroupChildren.clear();

    LODMap::const_iterator lodIt = _newLODChildren.begin();
    while (lodIt != _newLODChildren.end())
    {
        osg::LOD* lod = (*lodIt).first.get();

        const LODList& lodList = (*lodIt).second;
        LODList::const_iterator lit = lodList.begin();
        while( lit != lodList.end())
        {
            osg::Geode* geode = dynamic_cast< osg::Geode* >( (*lit)->getChild( 0 ) );
            float min = (*lit)->getMinRange( 0 );
            float max = (*lit)->getMaxRange( 0 );
            lod->addChild( geode, min, max );
            count++;
            lit++;
        }

        lodIt++;
    }
    _newLODChildren.clear();

    osg::notify( osg::NOTICE ) << "osgOQ: PerDrawableQueryVisitor: Moved " << count << " Drawables." << std::endl;
    return count;
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
PerDrawableQueryVisitor::apply( osg::Geode& geode )
{
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
            geode.removeDrawable( geom );

            unsigned int pIdx;
            for( pIdx=0; pIdx<geode.getNumParents(); pIdx++)
            {
                osg::Group* grp = geode.getParent( pIdx );
                osg::LOD* lod = dynamic_cast< osg::LOD* >( grp );
                if (lod != NULL)
                {
                    unsigned int childIdx = lod->getChildIndex( &geode );

                    osg::LOD* newLOD = new osg::LOD;
                    newLOD->addChild( newGeode.get(), lod->getMinRange( childIdx ), lod->getMaxRange( childIdx ) );
                    _newLODChildren[ lod ].push_back( newLOD );
                }
                else
                    _newGroupChildren[ grp ].push_back( newGeode.get() );
            }

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
    osg::OcclusionQueryNode* oqn = dynamic_cast<osg::OcclusionQueryNode*>( &node );
    if (oqn != NULL)
        oqn->setVisibilityThreshold( _visThreshold );
    traverse( node );
}

void
QueryFrameCountVisitor::apply( osg::Node& node )
{
    osg::OcclusionQueryNode* oqn = dynamic_cast<osg::OcclusionQueryNode*>( &node );
    if (oqn != NULL)
        oqn->setQueryFrameCount( _count );
    traverse( node );
}

void
EnableQueryVisitor::apply( osg::Node& node )
{
    osg::OcclusionQueryNode* oqn = dynamic_cast<osg::OcclusionQueryNode*>( &node );
    if (oqn != NULL)
        oqn->setQueriesEnabled( _enabled );
    traverse( node );
}


void 
DebugDisplayVisitor::apply( osg::Node& node )
{
    osg::OcclusionQueryNode* oqn = dynamic_cast<osg::OcclusionQueryNode*>( &node );
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

    osg::OcclusionQueryNode* oqnRaw = dynamic_cast<osg::OcclusionQueryNode*>( &node );
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
    osg::OcclusionQueryNode* oqn = dynamic_cast<osg::OcclusionQueryNode*>( &node );
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
#endif
