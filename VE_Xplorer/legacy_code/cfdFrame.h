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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFDFRAME_H
#define CFDFRAME_H

class cfdTransientVizHandler;
class cfdDataSet;
class cfdFILE;
class cfdTransientActor;
class cfdNode;

class vtkActor;
class vtkPolyData;

class cfdGeode;
class cfdDCS;
class cfdNode;

class cfdFrame
{
   public:
      cfdFrame();

      ~cfdFrame();

      //the data type
      enum FrameDataType{ VTK_SCALAR = 0, VTK_VECTOR, GEOM, 
                          VTK_PARTICLE, VTK_FLUENTPARTICLE };

      //the grid type
      enum FrameGeomDisplayType{VTK_SOLID = 0, VTK_WIRE};
      
      void CreateFrame( char * ); 
      void SetParameterFile( cfdTransientVizHandler *, int );
      void SetArrow( vtkPolyData * );
      void SetArrowSize( float );

      void SetActiveDataSets( cfdDataSet * activeDataSet,
                              cfdDataSet * activeMeshedVolume,
                              cfdDataSet * activeParticleData,
                              cfdDataSet * activeSurfaceData );

      cfdNode* GetcfdNode( void );

      void SetFrameDataType( int );
      int  GetFrameDataType();

   protected:
      int CreateFrame( void );

      cfdDCS*     dcs;
      cfdGeode*   geode;
      cfdNode*    node;

      vtkActor *actor;
      vtkPolyData * arrow;
      float arrowSize;

      cfdFILE *geomFile;
      cfdTransientVizHandler *param;

      cfdTransientActor * transientActor;

      char *vtkFilename;
      int displayType;
      int member;
      int frameDataType;
      cfdDataSet* activeDataSet;
      cfdDataSet* activeMeshedVolume;
      cfdDataSet* activeParticleData;
      cfdDataSet* activeSurfaceData;
};
#endif
