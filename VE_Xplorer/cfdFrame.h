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
 * File:          $RCSfile: cfdFrame.h,v $
 * Date modified: $Date: 2004/04/26 07:27:58 $
 * Version:       $Revision: 1.9 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFDFRAME_H
#define CFDFRAME_H

class cfdReadParam;
class cfdDataSet;
class cfdFILE;
class cfdTransientActor;

class vtkActor;
class vtkPolyData;

class pfGeode;
class pfDCS;
class pfNode;

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
      void SetParameterFile( cfdReadParam *, int );
      void SetArrow( vtkPolyData * );
      void SetArrowSize( float );

      void SetActiveDataSets( cfdDataSet * activeDataSet,
                              cfdDataSet * activeMeshedVolume,
                              cfdDataSet * activeParticleData,
                              cfdDataSet * activeSurfaceData );

      pfNode *GetpfNode( void );

      void SetFrameDataType( int );
      int  GetFrameDataType();

   protected:
      int CreateFrame( void );

      pfDCS   * dcs;
      pfGeode * geode;
      pfNode  * node;

      vtkActor *actor;
      vtkPolyData * arrow;
      float arrowSize;

      cfdFILE *geomFile;
      cfdReadParam *param;

      cfdTransientActor * transientActor;

      char *vtkFilename;
      int displayType;
      int member;
      int frameDataType;
      cfdDataSet * activeDataSet;
      cfdDataSet * activeMeshedVolume;
      cfdDataSet * activeParticleData;
      cfdDataSet * activeSurfaceData;
};
#endif
