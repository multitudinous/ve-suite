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
// --- VE-Suite Includes --- //
#include <ves/xplorer/environment/HeadPositionCallback.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <gmtl/Matrix.h>
#include <gmtl/Generate.h>
#include <gmtl/Misc/MatrixConvert.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/MatrixTransform>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace ves::xplorer::scenegraph;
namespace vxs = ves::xplorer::scenegraph;

using namespace ves::xplorer::environment;

////////////////////////////////////////////////////////////////////////////////
HeadPositionCallback::HeadPositionCallback()
{
    mHead.init( "VJHead" );
}
////////////////////////////////////////////////////////////////////////////////
HeadPositionCallback::~HeadPositionCallback()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
HeadPositionCallback::HeadPositionCallback( const HeadPositionCallback& ctc, const osg::CopyOp& copyop )
    :
    osg::NodeCallback( ctc, copyop )
{
    mHead.init( "VJHead" );
}
////////////////////////////////////////////////////////////////////////////////
void HeadPositionCallback::operator()(
    osg::Node* node, osg::NodeVisitor* nv )
{
    /*osg::ref_ptr< osg::Geode > mt =
        static_cast< osg::Geode* >( node );*/

    //Do work to place texture
    gmtl::Matrix44d vjHeadMat =
        gmtl::convertTo< double >( mHead->getData() );
 
    gmtl::Point3d jugglerHeadPoint =
        gmtl::makeTrans< gmtl::Point3d >( vjHeadMat );
    gmtl::Point3d startPoint;
    startPoint.set(
                   jugglerHeadPoint.mData[ 0 ] - 3.5f,
                   -jugglerHeadPoint.mData[ 2 ] + 10.0f,
                   jugglerHeadPoint.mData[ 1 ] + 0.0f);
    
    gmtl::Matrix44d worldMat = 
        vxs::SceneManager::instance()->GetInvertedWorldDCS();
    
    startPoint = worldMat * startPoint;
    gmtl::Quatd invertedQuat = gmtl::makeRot< gmtl::Quatd >( worldMat );
    osg::Quat quat;
    quat.set( invertedQuat.mData[ 0 ], invertedQuat.mData[ 1 ],
             invertedQuat.mData[2],invertedQuat.mData[ 3 ]);
    //We have to offset negative mX because the
    //view and frustum are drawn for the left eye
    static_cast< osg::PositionAttitudeTransform* >( node )->setPosition(
        osg::Vec3d( startPoint.mData[ 0 ], startPoint.mData[ 1 ], startPoint.mData[ 2 ] ) );

    static_cast< osg::PositionAttitudeTransform* >( node )->setAttitude( quat );
    
    traverse( node, nv );
}
////////////////////////////////////////////////////////////////////////////////
