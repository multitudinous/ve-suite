/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
 * File:          $RCSfile: cfdMenu.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_MENU_H
#define CFD_MENU_H

class vtkUnstructuredGridReader;
class vtkUnstructuredGrid;
class vtkCubeSource;
class vtkGeometryFilter;
class vtkPolyDataNormals;
class vtkPolyDataMapper;
class vtkPolyData;
class vtkActor;
class pfGeode;
class pfDCS;

#ifdef _CFDCOMMANDARRAY
class cfdApp;

#include "cfdCommandObjects.h"
#endif //_CFDCOMMANDARRAY

//! Create menu objects.
/*!
  A class to build a virtual menu using vtk functions and
  render using Performer.  VTK objects(vtkActor) are translated into
  Performer objects(pfGeode).
*/
class cfdMenu
#ifdef _CFDCOMMANDARRAY
                       : public cfdCommandObjects
#endif //_CFDCOMMANDARRAY
{
 public:
   //! Constructor
   /*!
    Construct VTK objects and Performer objects for menu.  
    Build using vtkUnstructuredGrid and vtkCube and translated into pfGeode(s).
    Initialize the menu.
    Load the menu data set, an unstructured grid, (POS_DATA/menu.vtk).
    Set the outline, shading, label, and active cell.
   */
   cfdMenu( char *, char * );

   //Destruct VTK objects and Performer objects for menu.
   ~cfdMenu();

#ifdef _CFDCOMMANDARRAY
   // compare VjObs_i commandArray with its child's value
   virtual bool CheckCommandId( cfdApp * _cfdApp );

   // in future, multi-threaded apps will make a copy of VjObs_i commandArray
   virtual void UpdateCommand();
#endif //_CFDCOMMANDARRAY

   /*!
    Return the dynamic coordinate system with pfGeode objects.
   */
   pfDCS * GetpfDCS();
   //!
   /*!
    Find the hit cell on menu and update.
   */
   void UpdateHitCell( double x[3] );
   //!
   /*!
    Return the boundary of the unstructured grid.
   */
   double * GetBound();
   //!
   /*!
    Get the active cell's id.
   */
   int GetCellId();


 private:
   //!
   /*!
    Highlight the cell, in which the laser is pointing.
   */
   void UpdateCell();
   //!
   /*!
    Get the white outline for the menu's cells.
   */
   pfGeode * GetOutline();
   //!
   /*!
    Get the transparent shading for the menu's cells.
   */
   pfGeode * GetShaded();
   //!
   /*!
    Get the text label for the menu's cells.
   */
   pfGeode * GetLabel( char *);
   //!
   /*!
    Get the active and highlighted menu's cell.
   */
   pfGeode * GetCell();
   //!
   /*!
    Get the polygons of the text for menu's cells.
   */
   vtkPolyData * GetText( char menuText[ ], double tPos[3] );
   //!
   /*!
    Active cell's id.
   */
   int id; 
   //!
   /*!
    Unstructured grid boundary.
   */
   double bound[6]; 
   //! VTK object
   /*!
    Menu's unstructured grid reader.
   */
   vtkUnstructuredGridReader *reader; 
   //! VTK object
   /*!
    Menu's unstructured grid reader.
   */
   vtkUnstructuredGrid *grid;
   //! VTK cube source object
   /*!
    Active cell vtk objects for rendering.
   */
   vtkCubeSource *cell;
   //! VTK cube source object
   /*!
    Active cell vtk objects for rendering.
   */
   vtkGeometryFilter *cellFilter;
   //! VTK cube source object
   /*!
    Active cell vtk objects for rendering.
   */
   vtkPolyDataNormals *cellNormal;
   //! VTK cube source object
   /*!
    Active cell vtk objects for rendering.
   */
   vtkPolyDataMapper *cellMapper;
   //! VTK cube source object
   /*!
    Active cell vtk objects for rendering.
   */
   vtkActor *cellActor;
   //! Performer object
   /*!
    pfGeode(s) for the menu.
   */
   pfGeode *outlineGeode;
   //! Performer object
   /*!
    pfGeode(s) for the menu.
   */
   pfGeode *shadedGeode;
   //! Performer object
   /*!
    pfGeode(s) for the menu.
   */
   pfGeode *labelGeode;
   //! Performer object
   /*!
    pfGeode(s) for the menu.
   */
   pfGeode *cellGeode;
   //! Performer object
   /*!
    Performer dynamic coordinate systems with pre-loaded translated VTK objects.
   */
   pfDCS *menuDCS;

};

#endif
