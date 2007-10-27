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
#ifndef CFD_CURSOR_H
#define CFD_CURSOR_H
/*!\file cfdCursor.h
cfdCursor API
*/
/*!\class VE_XPlorer::cfdCursor
*  A class to build virtual cursors. Type of virtual
*  cursor built are single point, arrow, and multiple points.
*/
#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/Group.h>
#include <ves/xplorer/scenegraph/Geode.h>

#include <ves/open/xml/CommandPtr.h>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
    class DCS;
    class Group;
    class Geode;
}
}
}

class vtkGlyph3D;
class vtkCubeSource;
class vtkSphereSource;
class vtkPolyData;
class vtkPolyDataNormals;
class vtkPolyDataMapper;
class vtkActor;
class vtkPointSource;
class vtkPlaneSource;
class vtkLineSource;
class vtkPolyDataSource;

#ifdef _OSG
#include <osg/ref_ptr>
#elif _PERFORMER
#endif

namespace VE_Xplorer
{
   class cfdCommandArray;
   class cfdDataSet;
}

#include <ves/xplorer/cfdGlobalBase.h>

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS cfdCursor : public cfdGlobalBase
{
public:
   cfdCursor( vtkPolyData* , ves::xplorer::scenegraph::DCS* , ves::xplorer::scenegraph::Group* );
   virtual ~cfdCursor();

   // compare VjObs_i commandArray with its child's value
   virtual bool CheckCommandId( VE_Xplorer::cfdCommandArray*  );

   // in future, multi-threaded apps will make a copy of VjObs_i commandArray
   virtual void UpdateCommand();

   void Initialize( double x[3], double v[3] );

   /*!
   Update the cursor's type(t), position(x), and
   direction(v).
   Type 0, multiple sphere cursor(x plane oriented).
   Type 1, multiple sphere cursor(y plane oriented).
   Type 2, multiple sphere cursor(z plane oriented).
   Type 3, single sphere cursor.
   Type 4, single arrow cursor.
   */
   /* 
   Update the position and direction of the virtual cursor
   based on the output from cfdNavigate::GetCursorLocation(location) 
   and cfdNavigate::GetDirection(direction).  
   */
   void Update( double x[3], double v[3], double wx[3] );

   vtkPolyData* GetSourcePoints( void );

   // Return the dynamic coordinate system with pfGeode objects.
   ves::xplorer::scenegraph::DCS* GetDCS();

   ///Set/Get plane size.
   void SetPlaneSize( float size );
   void GetPlaneSize( float &size );
   float GetPlaneSize();

   ///Set/Get plane resolution: the number of x-y subdivisions in the plane
   void SetPlaneReso( int size );
   void GetPlaneReso( int &size );
   int GetPlaneReso();

   ///Set the sphere scale
   ///\param scale The scale of the speheres
   void SetSphereScale( float scale );
   ///Set the cursor type
   ///\param The type of cursor to use
   void SetCursorType( int type );

   // for box cursor...
   void getExtent(double boxExtent[6]);
   vtkCubeSource *getBox();
   float boxExtent;

   void SetActiveDataSetDCS( ves::xplorer::scenegraph::DCS* myDCS );
   void SetActiveDataSet( cfdDataSet* input );

   int GetCursorID( void );
   double* GetCursorLocation( void );
   double* GetCursorLocalLocation( void );
   double* ReturnLocalLocationVector( void );
   void GetLocalLocationVector( void );
private:

   // Move the cursor methods
   void SetTranslation( void );
   void SetRotation( double [3] );

   void BuildSphere();           // Build sphere cursor.
   void BuildPlaneSource();      // Build nxn plane cursor
   // Update the current position of sphere cursor.
   void UpdateSphere();

   vtkSphereSource *sphereSrc;
   vtkPolyDataNormals *sphereNorm;
   vtkPolyDataMapper *sphereMapper;
   vtkActor *sphereActor;

   //Arrow Source Stuff
   // Build arrow cursor and use as source for glyph.
   void BuildArrowSource( void );
   void UpdateArrowSource( void );

   vtkPolyData * arrow;

   vtkPlaneSource * arrowPlaneS;
   vtkPolyDataMapper * arrowMapperS;
   vtkActor * arrowActorS;
   vtkGlyph3D * arrowGlyphS;
   //cfdDataSet * dataSet;

   // Line Source Stuff
   void UpdateLineSource( int );
   void BuildLineSource( void );

   vtkLineSource * lineSrc;
   vtkActor * lineActor;
   vtkPolyDataMapper *lineMapper;
   vtkGlyph3D * lineGlyph;
   vtkSphereSource * lineSphere;

   //add for box cursor
   void UpdateCube();
   void BuildCube();

   vtkCubeSource *cubeSrc;
   vtkPolyDataMapper *cubeMapper;
   vtkActor *cubeActor;

   // Plane Source Stuff
   // Build the x,y,z plane orientations cursors.
   void UpdatePlaneSource( int );

   vtkPlaneSource * planeSrc;
   vtkActor * planeActorS;
   vtkPolyDataMapper *planeMapperS;
   vtkGlyph3D * sphereGlyph;
   vtkSphereSource * planeSphereS;

   // I use 'pos' to indicate the current coordinate of the cursor
   double pos[3];
   double pos_c[3];

   // Performer dynamic coordinate systems with pre-loaded translated VTK objects.
   osg::ref_ptr< ves::xplorer::scenegraph::DCS > cursorDCS;
   osg::ref_ptr< ves::xplorer::scenegraph::DCS > cursorScaleDCS;
   osg::ref_ptr< ves::xplorer::scenegraph::DCS > worldDCS;

   // A Performer geometry node.
   osg::ref_ptr< ves::xplorer::scenegraph::Geode > cursorGeode;

   // Plane size;
   float pSize;
   float last_pSize;

   // Plane resolution;
   int pReso;
   int last_pReso;

   // Current position of cursor in virtual environment.
   double loc[3];

   // Current direction of cursor in virtual environment.
   double dir[3];

   // Current location of cursor relatice to worldDCS (Note: this is not to global 0,0,0).
   double localLocation[3];

   // last plane direction
   int last_direction;

   // last plane direction
   int last_cursor_type;

   osg::ref_ptr< ves::xplorer::scenegraph::DCS > activeDataSetDCS;

   osg::ref_ptr< ves::xplorer::scenegraph::Group > _rootNode;
   VE_Xplorer::cfdDataSet* _activeDataSet;
   int cursorId;
   float sphereRadius;
   float last_sphereRadius;
};
}
#endif
