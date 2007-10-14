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
#ifdef _OSG
#include <ves/xplorer/event/data/SeedPoints.h>
#include <osg/StateSet>
#include <osg/Shader>
#include <osg/Program>
#include <osg/BlendFunc>
#include <osg/PointSprite>
#include <osg/Geometry>
#include <osg/Point>
#include <osg/Node>
#include <osg/Uniform>
#include <iostream>
using namespace VE_Xplorer;
////////////////////////////////////////////////////////////////////////
SeedPoints::SeedPoints(unsigned int nX , unsigned int nY,
                              unsigned int nZ, 
							  float xmin,float xmax,
							  float ymin,float ymax,
							  float zmin,float zmax)
{
   unsigned int dimensions[3] = {1,1,1};
   dimensions[0] = nX;
   dimensions[1] = nY;
   dimensions[2] = nZ;

   float bounds[6] = {0.f,1.f,0.f,1.f,0.f,1.f};
   bounds[0] = xmin;
   bounds[1] = xmax;

   bounds[2] = ymin;
   bounds[3] = ymax;

   bounds[4] = zmin;
   bounds[5] = zmax;

   float color[3] = {1.f,1.f,0.f};

   _points = new PointsDrawable(dimensions,bounds);
   _points->SetColor(color);
   _pointSize = 10.f;
   _points->SetAlpha(1.0);
   _initializePoints();
   addDrawable(_points.get());
}
//////////////////////////////////////////////////////////////////////////////////////////
SeedPoints::SeedPoints(const SeedPoints& seedPoints,
                             const osg::CopyOp& copyop):
                             osg::Geode(seedPoints,copyop)
{
	_points = new PointsDrawable(*seedPoints._points);
	_pointSize = seedPoints._pointSize;
	_stateSet = seedPoints._stateSet;
	///_pointAttributes = new osg::Point(*seedPoints._pointAttributes);
}							 
/////////////////////////////////////////////////////
void SeedPoints::SetPointColor(float* color)
{
	if(_points.valid())
	{
		_stateSet->getUniform("pointColor")->set(osg::Vec4(color[0],color[1],color[2],color[3]));
		_points->SetColor(color);
	}
}
/////////////////////////////////////////////////////
void SeedPoints::SetPointAlpha(float alpha)
{
	if(_points.valid())
	{
		_points->SetAlpha(alpha);
	}
}
////////////////////////////////////////////////////////////////////////////
void SeedPoints::SetBounds(float xMin,float xMax,
                           float yMin,float yMax, 
                            float zMin,float zMax)
{
   if(_points.valid())
   {
      float bounds [6];
      bounds[0] = xMin;
	   bounds[1] = xMax;
	   bounds[2] = yMin;
	   bounds[3] = yMax;
	   bounds[4] = zMin;
	   bounds[5] = zMax;
	   _points->SetBounds(bounds);
	}
}
////////////////////////////////////////////////////////////////
void SeedPoints::SetDimensions(unsigned int numX,
                                        unsigned int numY,
                                        unsigned int numZ)
{
   if(_points.valid())
   {
     unsigned int dimensions[3];
     dimensions[0] = numX;
	  dimensions[1] = numY;
	  dimensions[2] = numZ;
	  _points->SetDimensions(dimensions);
   }
}
//////////////////////////////////////////////////
void SeedPoints::SetPointSize(float size)
{
   if(_pointAttributes)
   {
	  _pointSize = size;
      _pointAttributes->setSize(_pointSize);
	  //_stateSet->dirty();
   }
}
//////////////////////////////////////////
void SeedPoints::_initializePoints()
{
   _stateSet = new osg::StateSet();

   /// Setup cool blending
   _stateSet->setMode(GL_BLEND, osg::StateAttribute::ON);
   osg::ref_ptr<osg::BlendFunc> fn = new osg::BlendFunc();
   //fn->setFunction(osg::BlendFunc::SRC_ALPHA, osg::BlendFunc::DST_ALPHA);
   fn->setFunction(osg::BlendFunc::SRC_ALPHA, osg::BlendFunc::ONE_MINUS_SRC_ALPHA);
   _stateSet->setAttributeAndModes(fn.get(), osg::StateAttribute::ON);

   /// Setup the point sprites
   osg::ref_ptr<osg::PointSprite> sprite = new osg::PointSprite();
   _stateSet->setTextureAttributeAndModes(0, sprite.get(), osg::StateAttribute::ON|osg::StateAttribute::PROTECTED);

   /// Give some size to the points to be able to see the sprite
   _pointAttributes = new osg::Point();
   _pointAttributes->setSize(_pointSize);
   _stateSet->setAttribute(_pointAttributes);

   /// Disable depth test to avoid sort problems and Lighting
   //_stateSet->setMode(GL_DEPTH_TEST, osg::StateAttribute::OFF|osg::StateAttribute::PROTECTED);
   //_stateSet->setMode(GL_LIGHTING, osg::StateAttribute::OFF|osg::StateAttribute::PROTECTED);
   _stateSet->setRenderingHint(osg::StateSet::TRANSPARENT_BIN);
   //_stateSet->setBinNumber(99);
	osg::ref_ptr<osg::Uniform> pointColor = new osg::Uniform("pointColor",
                                                             osg::Vec4(1.0,1.0,0.0,.7));
    // frag shader for the points
    char fragShaderSource[] = 
	"uniform vec4 pointColor;\n"
    "void main(void) \n"
    "{ \n"
    "\n"
	"gl_FragColor = (step(.25,distance(gl_TexCoord[0].xy,vec2(.5,.5)))==1.0)?vec4(0,0,0,0):pointColor;\n"
    "}\n";
   osg::ref_ptr<osg::Program> program = new osg::Program;
   _stateSet->setAttribute(program.get());

   osg::ref_ptr<osg::Shader> fragShader = new osg::Shader(osg::Shader::FRAGMENT, 
                                                                           fragShaderSource);
   program->addShader(fragShader.get());
   _points->setStateSet(_stateSet.get());
   _stateSet->addUniform(pointColor.get());
   //setStateSet(_stateSet.get());
}
//////////////////////////////////////////////
void SeedPoints::Toggle(bool onOff)
{
	setNodeMask((onOff)?1:0);
}
///////////////////////////////////////////////////////////////////////////////
void SeedPoints::UpdateBounds(double newBoundsValue,
                              std::string coordinate,
                              std::string minMax)
{
   unsigned int indexShift = (coordinate=="X")?0:(coordinate=="Y")?2:4;
   unsigned int index = (minMax=="Min")?indexShift:indexShift+1;
   _points->UpdateBound(index,newBoundsValue);
}
/////////////////////////////////////////////////////////////////////////////
///PointsDrawable class
 ///Constructor
/////////////////////////////////////////////////////////////////////////////////////////////////////////
SeedPoints::PointsDrawable::PointsDrawable(unsigned int* dimensions, float* bounds)
{
	setUseDisplayList(false);
   _dimensions[0] = dimensions[0];
   _dimensions[1] = dimensions[1];
   _dimensions[2] = dimensions[2];

   _bounds[0] = bounds[0];
   _bounds[1] = bounds[1];

   _bounds[2] = bounds[2];
   _bounds[3] = bounds[3];

   _bounds[4] = bounds[4];
   _bounds[5] = bounds[5];

   _color[0] = 1.f;
   _color[1] = 1.f;
   _color[2] = 0.f;
   _color[3] = 1.f;
}
///Copy Constructor
////////////////////////////////////////////////////////////////////////////////////////////////////////
SeedPoints::PointsDrawable::PointsDrawable(const SeedPoints::PointsDrawable& pointsDrawable,
										               const osg::CopyOp& copyop)
													   :osg::Geometry(pointsDrawable,copyop)
{
   _dimensions[0] = pointsDrawable. _dimensions[0];
   _dimensions[1] = pointsDrawable. _dimensions[1];
   _dimensions[2] = pointsDrawable. _dimensions[2];

   _bounds[0] = pointsDrawable._bounds[0];
   _bounds[1] = pointsDrawable._bounds[1];

   _bounds[2] = pointsDrawable._bounds[2];
   _bounds[3] = pointsDrawable._bounds[3];

   _bounds[4] = pointsDrawable._bounds[4];
   _bounds[5] = pointsDrawable._bounds[5];

   _color[0] = pointsDrawable._color[0];
   _color[1] = pointsDrawable._color[1];
   _color[2] = pointsDrawable._color[2];
   _color[3] = pointsDrawable._color[3];
}
/////////////////////////////////////////////////////////////////////////
void SeedPoints::PointsDrawable::SetBounds(float* bounds)
{
	_bounds[0] = bounds[0];
	_bounds[1] = bounds[1];
	_bounds[2] = bounds[2];
	_bounds[3] = bounds[3];
	_bounds[4] = bounds[4];
	_bounds[5] = bounds[5];
	dirtyDisplayList();
}
/////////////////////////////////////////////////////////////////////////////////////////////
void SeedPoints::PointsDrawable::SetDimensions(unsigned int* dimensions)
{
	_dimensions[0] = dimensions[0];
	_dimensions[1] = dimensions[1];
	_dimensions[2] = dimensions[2];
	dirtyDisplayList();
}
///////////////////////////////////////////////////////////////
/*void SeedPoints::PointsDrawable::SetSize(float size)
{
	_pointSize = size;
}*/
////////////////////////////////////////////////////////////////////
void SeedPoints::PointsDrawable::SetColor(float* color)
{
	_color[0] = color[0];
	_color[1] = color[1];
	_color[2] = color[2];
	dirtyDisplayList();
}
/////////////////////////////////////////////////////////////////////////////
void SeedPoints::PointsDrawable::UpdateBound(unsigned int index,double value)
{
   if(index <6)
   {
      _bounds[index] = value;
      return;
   }
   std::cout<<"Invalid Boundary index: "<<index<<std::endl;
   std::cout<<"SeedPoints::PointsDrawable::UpdateBound()"<<std::endl;
}
///////////////////////////////////////////////////////////////////
void SeedPoints::PointsDrawable::SetAlpha(float alpha)
{
	_color[3] = alpha;
	dirtyDisplayList();
}
//////////////////////////////////////////////////////////////////////////////////////
osg::BoundingBox SeedPoints::PointsDrawable::computeBound() const
{
	return osg::BoundingBox(_bounds[0],_bounds[1],
		                     _bounds[2],_bounds[3],
								   _bounds[4],_bounds[5]);
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////
void SeedPoints::PointsDrawable::drawImplementation(osg::State& currentState) const
{
	unsigned int numSeedPoints = _dimensions[0]*_dimensions[1]*_dimensions[2];
	float dx = (_dimensions[0]==1)?0:(_bounds[1]-_bounds[0])/(_dimensions[0]-1);
	float dy = (_dimensions[1]==1)?0:(_bounds[3]-_bounds[2])/(_dimensions[1]-1);
	float dz = (_dimensions[2]==1)?0:(_bounds[5]-_bounds[4])/(_dimensions[2]-1);

	float x = _bounds[0];
	float y = _bounds[2];
	float z = _bounds[4];

   glBegin(GL_POINTS);
   for (unsigned int i = 0; i < numSeedPoints; i++) 
   {
	   x = _bounds[0] + dx*(i%_dimensions[0]);
	   y = _bounds[2] + dy*((i / _dimensions[0])% _dimensions[1]);
	   z = _bounds[4] + dz*(i/(_dimensions[0]*_dimensions[1]));
	   //glColor4fv(_color);
	   glVertex3f(x,y,z);
	   
    }
	glEnd();
}
#endif

