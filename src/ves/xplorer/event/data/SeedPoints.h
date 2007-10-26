/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef VE_SEED_POINTS_H
#define VE_SEED_POINTS_H
#ifdef _OSG
#include <osg/Geode>
#include <osg/Geometry>
#include <ves/VEConfig.h>
#include <osg/Version>

#if ((OSG_VERSION_MAJOR>=1) && (OSG_VERSION_MINOR>2) || (OSG_VERSION_MAJOR>=2))
#include <osg/RenderInfo>
#endif

namespace osg
{
	class StateSet;
	class Geometry;
	class Point;
}
/*!\file SeedPoints.h
  Seed Points 
  */

/*!\class VE_Xplorer::SeedPoints
 * Class defining the a volume of points
 */
namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS SeedPoints: public osg::Geode
{
public:
	///Constructor
	///\param numX The number in X.
    ///\param numY The number in Y.
    ///\param numZ The number in Z.
	///\param xMin Minimum x
    ///\param yMin Minimum y
    ///\param zMin Minimum z
    ///\param xMax Maximum x
    ///\param yMax Maximum y
    ///\param zMax Maximum z
	SeedPoints(unsigned int nX = 1,
		           unsigned int nY = 1,
				   unsigned int nZ =1,
		           float xmin = 0.f,
				   float xmax =1.f,
				   float ymin = 0.f,
				   float ymax = 1.f,
				   float zmin = 0.f,
				   float zmax = 1.f);
	///Copy Constructor
    SeedPoints(const SeedPoints& seedPoints,
               const osg::CopyOp& copyop=osg::CopyOp::SHALLOW_COPY);

   META_Object(VE_Xplorer,SeedPoints)
   ///Set the bounds
   ///\param xMin Minimum x
   ///\param yMin Minimum y
   ///\param zMin Minimum z
   ///\param xMax Maximum x
   ///\param yMax Maximum y
   ///\param zMax Maximum z
   void SetBounds(float xMin,float xMax,float yMax,float yMin, 
                   float zMin,float zMax);
   
   ///Set the dimensions of the seed point volume
   ///\param numX The number in X.
   ///\param numY The number in Y.
   ///\param numZ The number in Z.
   void SetDimensions(unsigned int numX,
                            unsigned int numY,
                            unsigned int numZ);

   ///Set the size of the points
   ///\param size Pixel size of points
   void SetPointSize(float size=10);

   ///Set the RGB values for the seed points
   ///\param color The rgb values for the points
   void SetPointColor(float* color);

   /// Set the transparency of the points
   ///\param alpha The alpha(transparency) of the points  
   void SetPointAlpha(float alpha);

   ///Turn the node display off/on
   ///\param onOff Flag to determine on off of the node.
   void Toggle(bool onOff);

   ///Update a boundary for the bbox
   ///\param newBoundsValue The new value infomation
   ///\param coordinate The coordinate direction to update\n "X","Y","Z"
   ///\param minMax Indicates which value to update "Min" or "Max"
   void UpdateBounds(double newBoundsValue,
                     std::string coordinate="X",
                     std::string minMax="Min");

protected:
   class PointsDrawable: public osg::Geometry
   {
   public:
	   ///Constructor
	   ///\param dimensions The xyz dimensions of the volume
	   ///\param bounds The bbox of the volume
	   PointsDrawable(unsigned int* dimensions=0,float* bounds=0);

	   ///Copy Constructor
	   PointsDrawable(const PointsDrawable& pointsDrawable,
                           const osg::CopyOp& copyop=osg::CopyOp::SHALLOW_COPY);

	   META_Object(VE_Xplorer,PointsDrawable)
	   ///Set the bounding box of the point volume
	   ///\parm bounds The bounding box\n xmin,xmax,ymin,ymax,zmin,zmax
	   void SetBounds(float* bounds);

	   ///Set the xyz dimensions
	   ///\param dimensions x,y,z size of the point volume
	   void SetDimensions(unsigned int* dimensions);

	   ///Set the size for the seed points in pixels
	   ///\param size Size in pixels
	   //void SetSize(float size);

	   ///Set the rgb color for the points
	   ///\param color The rgb color
	   void SetColor(float* color); 

	   ///Set the alpha value
	   ///\param alpha Transparency
	   void SetAlpha(float alpha);
	  
	   /// we need to set up the bounding box of the data too, so that the scene graph knows where this
           /// objects is, for both positioning the camera at start up, and most importantly for culling.
           virtual osg::BoundingBox computeBound() const;

         ///the draw immediate mode method is where the OSG wraps up the drawing of
        /// of OpenGL primitives.
#if ((OSG_VERSION_MAJOR>=1) && (OSG_VERSION_MINOR>2) || (OSG_VERSION_MAJOR>=2))
        virtual void drawImplementation(osg::RenderInfo& currentState) const;
#elif ((OSG_VERSION_MAJOR<=1) && (OSG_VERSION_MINOR<=2))
        virtual void drawImplementation(osg::State& currentState) const;
#endif

      

      ///Update a specific boundary
      ///\param index The index of the boundary
      ///\param value The new value
      void UpdateBound(unsigned int index,double value);
protected:
      unsigned int _dimensions[3];///<The dimensions
      float _bounds[6];///<The bounding box
      float _color[4];///<The color properties of the points
   };
   ///Destructor
   virtual ~SeedPoints(){};
   float _pointSize;///<The size of the points
   osg::ref_ptr<osg::StateSet> _stateSet;///<The stateset
   osg::ref_ptr<PointsDrawable> _points;///<The points
   //osg::ref_ptr<osg::Point> _pointAttributes;///<The state attribute for the points;
   osg::Point* _pointAttributes;///<The state attribute for the points;
   ///Initialize the object for rendering 
   void _initializePoints();
 
};
}
#endif//_OSG
#endif// VE_SEED_POINTS_H
