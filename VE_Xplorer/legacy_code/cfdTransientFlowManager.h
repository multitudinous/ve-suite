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
#ifndef _VRAC_TRANSIENT_FLOW_MANAGER_H_
#define _VRAC_TRANSIENT_FLOW_MANAGER_H_

//class to handle the transient data

#include "cfdObjects.h"
#include <vector>

class cfdReadParam;
class cfdFrame;
class cfdTransientVizHandler;

class cfdTransientFlowManager : public cfdObjects
{
public:
   cfdTransientFlowManager();
   ~cfdTransientFlowManager();

   // Stuff inherited from cfdObjects
   virtual void Update( void );   

   // this allows the TFM to pass parameter file info down to frames 
   void SetParameterFile( cfdTransientVizHandler* param, int );

   void SetDirectory( char * );
   char * GetDirectory();
   
   int GetNumberOfFrames( void );

   int StoreFrameFileNames( void );

   //internal loading of frames
   void LoadFrames( void );

   void SetFrameDataType( int );
   int  GetFrameDataType();

   int isTimeToUpdateSequence;
   void CreateNodeList( void );
protected:
   int member;
   char * directory;
   int frameDataType;
   int numFrames;
   cfdFrame* frames;
   std::vector< char* > frameFileNames;
   int * order;
   // Fix this 
   // this classs needs major rework fix
   cfdTransientVizHandler* paramFile;
};
#endif //_VRAC_TRANSIENT_FLOW_MANAGER_H_
