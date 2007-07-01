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

#include "osgOQ/CustomRenderBin.h"
#include <osg/BoundingBox>
#include <osgUtil/RenderBin>


using namespace osgOQ;


// Create a new RenderBin to store Drawables depth-sorted front to back.
osgUtil::RegisterRenderBinProxy s_customRenderBinProxy(
	"FrontToBackAndOutToIn", new CustomRenderBin() );


CustomRenderBin::CustomRenderBin()
  : osgUtil::RenderBin( osgUtil::RenderBin::SORT_FRONT_TO_BACK )
{
}
CustomRenderBin::CustomRenderBin( const CustomRenderBin& rhs, const osg::CopyOp& copyop )
  : osgUtil::RenderBin( rhs, copyop )
{
}

CustomRenderBin::~CustomRenderBin()
{
}

struct F2BO2ISortFunctor
{
	// Sort outside-to-inside and front-to-back for optimum occlusion culling.
	bool operator()( const osgUtil::RenderLeaf* lhs, const osgUtil::RenderLeaf* rhs ) const
    {
		// Computes the distance from the eye to the nearest point on the
		//   bounding volume for the LHS and RHS. Returns true if the LHS
		//   distance is less than the RHS distance.
		// If RHS is inside LHS, this returns true.
		// If LHS is inside RHS this returns false.
		// If no inclusion and LHS closer than RHS, returns true.
        return( (lhs->_depth - lhs->_drawable->getBound().radius()) <
			(rhs->_depth - rhs->_drawable->getBound().radius()) );

    }
};
void
CustomRenderBin::sortImplementation()
{
    copyLeavesFromStateGraphListToRenderLeafList();
    std::sort( _renderLeafList.begin(), _renderLeafList.end(), F2BO2ISortFunctor() );
}
