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

#ifndef NURBS_CONTROL_MESH_H
#define NURBS_CONTROL_MESH_H

#include <ves/VEConfig.h>
#include <osg/Geometry>
#include <osg/Array>
namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace nurbs
{
class ControlPoint;
class VE_NURBS_EXPORTS NURBSControlMesh: public osg::Geometry
{
public:
    ///Constructor
    NURBSControlMesh();

    ///Constructor
    ///\param controlPoints The control points of the NURBS 
    ///\param numU The number of control points in the U direction 
    ///\param numV The number of control points in the V direction 
    ///\param isSurface Flag describing type of NURBS 
    NURBSControlMesh( std::vector<ves::xplorer::scenegraph::nurbs::ControlPoint> controlPoints,
                      unsigned int numU,
                      unsigned int numV, bool isSurface = false );

    ///Copy constructor
    NURBSControlMesh( const NURBSControlMesh& controlMesh,
                      const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    META_Object( NURBS, ves::xplorer::scenegraph::nurbs::NURBSControlMesh )

    ///Set the control points
    ///\param controlPoints The control points of the NURBS 
    ///\param numU The number of control points in the U direction 
    ///\param numV The number of control points in the V direction 
    ///\param isSurface Flag describing type of NURBS 
    void SetControlPoints( std::vector<ves::xplorer::scenegraph::nurbs::ControlPoint> controlPoints,
                           unsigned int numU,
                           unsigned int numV, bool isSurface = false );
    
    ///Update the position of a specific ControlPoint
    ///\param controlPointIndex The index of the ControlPoint to move
    ///\param position The new position of the ControlPoint
    void UpdateControlPointPosition( int controlPointIndex, osg::Vec3 position );

protected:
    ///Destructor
    virtual ~NURBSControlMesh(){}
   
    ///Update the control point primative verts
    void _updateControlMeshPrimitives();
    ///Update the points
    void _updateControlPointsPrimitives();
    ///Update the u iso-curves
    void _updateControlUCurvePrimitives();
    ///Update the v iso-curves
    void _updateControlVCurvePrimitives();

    ///Initialize the point properties
    void _initializeStateSet();

    bool m_isSurface;///<Flag for determining NURBS type.
    unsigned int m_numUControlPoints;///<The number of control points in U direction.
    unsigned int m_numVControlPoints;///<The number of control points in V direction.
    std::vector<ves::xplorer::scenegraph::nurbs::ControlPoint> m_controlPoints;///<The control points
    ///The osg points...need to migrate to not have the extra copy t
    osg::ref_ptr<osg::Vec3Array> m_controlMeshVerts;
};
}
}
}
}
#endif //NURBS_CONTROL_MESH_H

