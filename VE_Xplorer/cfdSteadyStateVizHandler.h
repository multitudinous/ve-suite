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
 * File:          $RCSfile: cfdSteadyStateVizHandler.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_STEADYSTATEVIZHANDLER_H
#define CFD_STEADYSTATEVIZHANDLER_H

#include <vector>
#include <map>

#include <vpr/Thread/Thread.h>

class cfdPolyData;      
class cfdIsosurface;    
class cfdPresetContour; 
class cfdContours;      
class cfdMomentum;      
class cfdPresetMomentum;
class cfdMomentums;     
class cfdVector;        
class cfdPresetVector;  
class cfdVectors;       
class cfdStreamers;     
class cfdPolyData;      
class cfdImage;         
class cfdAnimatedImage; 
class cfdAnimatedStreamlineCone;
class cfdContour;
class cfdDataSet;
class cfdGlobalBase;
class cfdObjects;
class cfdCommandArray;
class cfdDCS;
class vtkPolyData;
class cfdNavigate;
class cfdCursor;
class cfdTempAnimation;
class cfdGraphicsObject;
class cfdSteadyStateVizHandler
{
   public:
      cfdSteadyStateVizHandler( char* );
      ~cfdSteadyStateVizHandler( void );

      void InitScene( void );
      void PreFrameUpdate( void );
      void CreateActorThread( void * );
      void streamers( void );

      // Helper functions
      void SetActiveDataSet( cfdDataSet* );
      void SetCommandArray( cfdCommandArray* );
      void SetWorldDCS( cfdDCS* );
      void SetNavigate( cfdNavigate* );
      void SetCursor( cfdCursor* );
      cfdTempAnimation* GetActiveAnimation( void );
      bool TransientGeodesIsBusy();
 

   private:
      cfdPolyData*         surface;
      cfdIsosurface*       isosurface;
      cfdContour*          contour;
      cfdPresetContour*    x_contour;
      cfdPresetContour*    y_contour;
      cfdPresetContour*    z_contour;
      cfdContours*         x_contours;
      cfdContours*         y_contours;
      cfdContours*         z_contours;
      cfdMomentum*         momentum;
      cfdPresetMomentum*   x_momentum;
      cfdPresetMomentum*   y_momentum;
      cfdPresetMomentum*   z_momentum;
      cfdMomentums*        x_momentums;
      cfdMomentums*        y_momentums;
      cfdMomentums*        z_momentums;
      cfdVector*           vector;
      cfdPresetVector*     x_vector;
      cfdPresetVector*     y_vector;
      cfdPresetVector*     z_vector;
      cfdVectors*          x_vectors;
      cfdVectors*          y_vectors;
      cfdVectors*          z_vectors;
      cfdStreamers*        streamlines;
      cfdPolyData*         particles;
      cfdImage*            image;
      cfdAnimatedImage*    animImg;
      cfdAnimatedStreamlineCone* animStreamer;

      // Common objects for all functions
      cfdDataSet* _activeDataSet;
      cfdCommandArray*  commandArray;
      cfdDCS*     _worldDCS;
      cfdDCS*     _activeDataSetDCS;
      cfdObjects* _activeObject;
      cfdTempAnimation* _activeTempAnimation;

      // Classes and variables for multithreading.
      vpr::ThreadMemberFunctor< cfdSteadyStateVizHandler >* vjThFunc[1];
      vpr::Thread* vjTh[1];
   
      // Vectors that will eventually be stored as maps
      // these hold all the objectsa for easy access and management
      std::vector< cfdObjects* > dataList;
      std::vector< cfdGlobalBase* > commandList;

      char* _param;
      bool actorsAreReady;
      bool computeActorsAndGeodes;
      bool changeGeometry;
      vtkPolyData*   lastSource;
      cfdNavigate*   nav;
      cfdCursor*     cursor;
      // Stores data from cfdCursor
      // Variable will eventually be used to define bounding box
      // for data interagation
      double cur_box[6];

      // Need to get rid of this bool fix 
      //bool inter_activeObject;
      //bool chgMod;
      //bool runStreamersThread;
      bool runIntraParallelThread;
      bool useLastSource;
      bool transientBusy;

      // multi map to hold graphics objects
      // the key is the viz type and the value is cfdGraphicsObject
      std::multimap< int, cfdGraphicsObject* > graphicsObjects;
};
#endif
