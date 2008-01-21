/*************** <auto-copyright.pl BEGIN do not edit this line> *************
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
 * Date modified: $Date: 2007-12-26 14:42:01 -0600 (Wed, 26 Dec 2007) $
 * Version:       $Rev: 10265 $
 * Author:        $Author: biv $
 * Id:            $Id: NURBSNode.cxx 10265 2007-12-26 20:42:01Z biv $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> **************/
#ifndef NURBS_DRAWABLE_H
#define NURBS_DRAWABLE_H
#include <osg/Geometry>
#include <ves/VEConfig.h>

#include <iostream>
/*!\file NURBSDrawable.h
  Drawable for NURBSObject 
  */
/*!\file NURBSDrawable.cxx
  Drawable for NURBSObject Renderer code
  */
/*!\class ves::xplorer::scenegraph::nurbs::NURBSDrawable
 * Class defining the interface between NURBS object and osg::Geometry.
 */


namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace nurbs
{
class NURBSObject;
class VE_NURBS_EXPORTS NURBSDrawable: public osg::Geometry
{
public:
    ///Constructor
    NURBSDrawable( ves::xplorer::scenegraph::nurbs::NURBSObject* nurbsObject = 0 );

    ///Copy constructor
    NURBSDrawable( const NURBSDrawable& tessSurf,
                   const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    META_Object( VE_NURBS, ves::xplorer::scenegraph::nurbs::NURBSDrawable )

    ///Update the mesh based on a moved ControlPoint
    ///\param changedPoints The indecies of the changed points 
    ///\param nurbs The updated NURBSObject 
    void UpdateMesh( ves::xplorer::scenegraph::nurbs::NURBSObject* nurbs );

    ///Get the raw NURBS data
    ves::xplorer::scenegraph::nurbs::NURBSObject* GetNURBSData();

protected:
    ///Destructor
    virtual ~NURBSDrawable()
    {}

    ///Calculate the normal on the surface at point.
    ///\param index Index of the surface point to calculate a normal 
    osg::Vec3 _calculateSurfaceNormalAtPoint( unsigned int index );
 
    ///Update the vbos
    void _updateTessellatedSurface();

     ///Update the surface vbo
     void _updateSurfacePrimitive();

     ///Update the curve vbo
     void _updateCurvePrimitive();
     
     ///Initialize the StateSet for the drawable
     void _initializeStateSet();

     ves::xplorer::scenegraph::nurbs::NURBSObject* m_nurbsObject;///The NURBS representation
     osg::ref_ptr<osg::Vec3Array> m_tessellatedPoints;///<The points to tessellate
     osg::ref_ptr<osg::Vec3Array> m_normals;///<The points to tessellate
     osg::ref_ptr<osg::Vec2Array> m_texCoords;///<The points to tessellate
};
}
}
}
}
#endif//NURBS_DRAWABLE_H
