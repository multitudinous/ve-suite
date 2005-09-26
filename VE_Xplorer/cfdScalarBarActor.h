/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: cfdScalarBarActor.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_SCALAR_BAR_ACTOR_H
#define CFD_SCALAR_BAR_ACTOR_H

#include "VE_Xplorer/cfdGlobalBase.h"
//#include <string>

namespace VE_SceneGraph
{
   class cfdDCS;
   class cfdGeode;
   class cfdGroup;
}
class vtkLookupTable;
class vtkVectorText;

namespace VE_Xplorer
{
   class cfdCommandArray;
   class cfdDataSet;
   class cfdReadParam;
}

//! VTK scalar bar render
/*!
  A rebuilt class from vtkScalarBarActor(2-D) for use in the
  3-D space.
*/
namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS cfdScalarBarActor : public cfdGlobalBase
{
public:
   cfdScalarBarActor( char*, VE_SceneGraph::cfdGroup* );
   ~cfdScalarBarActor();

   // compare VjObs_i commandArray with its child's value
   virtual bool CheckCommandId( cfdCommandArray * _cfdCommandArray );

   // in future, multi-threaded apps will make a copy of VjObs_i commandArray
   virtual void UpdateCommand();

   // Create the scalar bar
   void RefreshScalarBar( void );
   // Set the active dataset for scalar bar computations
   void SetActiveDataSet( cfdDataSet* );
   // Read parameter file
   void CreateObjects( void );

   // Set/Get the position of the scalar bar in 3D space
   void SetPosition(float x, float y, float z);
   void SetPosition(float x[3]);
   void GetPosition(float x[3]);
   void GetPosition(float &x, float &y, float &z);

   void SetZRotation( float );

   // Set/Get the width of the scalar bar
   void SetWidth(float w);
   float GetWidth() const;

   // Set/Get the height of the scalar bar
   void SetHeight(float h);
   float GetHeight() const;

   // Set/Get the number of colors for the scalar bar
   void SetMaximumNumberOfColors(int nC);
   int GetMaximumNumberOfColors() const;

   // Set/Get the range of the scalar bar
   void SetRange(double r0, double r1);
   void SetRange(double r[2]);
   void GetRange(double r[2]);
   void GetRange(double &r0, double &r1);

   // Set/Get the lookup table of the scalar bar
   void SetLookupTable( vtkLookupTable * );
   vtkLookupTable * GetLookupTable();

   // Set/Get the scale of the title text
   void SetTitleTextScale(float scale);
   float GetTitleTextScale() const;

   // Set/Get the text for the scalar bar
   void SetVtkVectorText(char text[]);

   // Create the scalar bar and convert it into pfGeode
   void Execute();

   VE_SceneGraph::cfdDCS * GetcfdDCS( void);

private:
   float itsX[3];
   float zrot;
   float width, height;
   int numColors;
   vtkLookupTable* lut;
   vtkVectorText* titleScalar;
   int numPts;
   double range[2];
   float dScalar;
   float titleTextScale;
   VE_SceneGraph::cfdGeode* pfaPolyActor;
   VE_SceneGraph::cfdGeode* pftitleActor;
   VE_SceneGraph::cfdGeode* pfLabelActor[5];
   int numTextLabels;   // number of numerical labels on the scalar bar legend
   VE_SceneGraph::cfdGeode* cubeAxesGeode;

   VE_SceneGraph::cfdDCS*  scalarBar;
   VE_SceneGraph::cfdGroup* _rootNode;
   char* _param;
   cfdDataSet* _activeDataSet;
   float scalarBarPos[ 3 ];
   float scalarBarZRot;
   float scalarBarH;
   float scalarBarW;
   cfdReadParam* _readParam;
   double realOpacity;
};

}
#endif
