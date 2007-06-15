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
#ifndef CFD_TRANSIENTVIZHANDLER_H
#define CFD_TRANSIENTVIZHANDLER_H

#include <vector>
#include <map>
#include <string>

class cfdTransientFlowManager;
class cfdObjects;
class cfdAnimation;
class cfdTransientInfo;
class cfdReadParam;
class cfdDCS;
class cfdDataSet;
class cfdCommandArray;

class vtkPolyData;

class cfdTransientVizHandler
{
   public:
      cfdTransientVizHandler( char* );
      ~cfdTransientVizHandler( void );

      void InitScene( void );
      void PreFrameUpdate( void );
      void CreateObjects( void );
      void SetCommandArray( cfdCommandArray* );
      void CreateActors( void );

      // All of these functions should disapear and be fixed
      // They are a MAJOR hack
      void CreateNewDataSet( void );
      int GetNumberOfDataSets( void );
      cfdDataSet* GetDataSet( int );
      cfdDataSet* GetDataSetWithName( const char* );

      // Need to move this back to protected once trans junk is cleaned up
      std::vector< cfdTransientInfo * > transientInfo;
   private:
      cfdTransientFlowManager* _cfdTFM_X_Contour[2];//added for windshield hack
      cfdTransientFlowManager* _cfdTFM_Y_Contour;
      cfdTransientFlowManager* _cfdTFM_Z_Contour;
      cfdTransientFlowManager* _cfdTFM_X_Vector;
      cfdTransientFlowManager* _cfdTFM_Y_Vector;
      cfdTransientFlowManager* _cfdTFM_Z_Vector;
      cfdTransientFlowManager* _cfdTFM_Geometry[2];
      cfdTransientFlowManager* _cfdTFM_Particle;
      cfdAnimation*           transientSequence;
      std::vector< cfdObjects * > dataList;
      std::vector< cfdDataSet * > dataSets;
      cfdReadParam* _readParam;
      vtkPolyData*   lastSource;
      cfdDCS* _activeDataSetDCS;
      cfdObjects* _activeObject;
      cfdObjects* _activeSequenceObject;
      cfdCommandArray* _commandArray;
      bool computeActorsAndGeodes;
      char* _param;
      std::vector< cfdCommandArray* > commandList;
      std::map< int, std::string > mapDataKeyToName;
};
#endif
