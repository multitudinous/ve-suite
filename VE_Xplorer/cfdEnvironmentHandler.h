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
 * File:          $RCSfile: cfdEnvironmentHandler.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_ENVIRONMENTHANDLER_H
#define CFD_ENVIRONMENTHANDLER_H

class cfdNavigate;
//class cfdLaser;
//class cfdMenu;
class cfdCursor;
class cfdDCS;
class cfdGroup;
class cfdCommandArray;
class cfdReadParam;
class cfdSoundHandler;
class cfdTeacher;

class vtkPolyData;

class cfdEnvironmentHandler
{
   public:
      cfdEnvironmentHandler( char* );
      ~cfdEnvironmentHandler( void );

      void InitScene( void );
      void PreFrameUpdate( void );
      void SetRootNode( cfdGroup* );
      void SetWorldDCS( cfdDCS* );
      void SetCommandArray( cfdCommandArray* );
      void SetArrow( vtkPolyData* );
      void CreateObjects( void );
      cfdNavigate* GetNavigate( void );
      cfdCursor* GetCursor( void );

      cfdSoundHandler* GetSoundHandler( void );

      cfdTeacher* GetTeacher( void );
   private:
      cfdNavigate* nav;
      cfdSoundHandler* _soundHandler;
      //cfdLaser* laser;
      //cfdMenu* menu;
      cfdCursor* cursor;
      char* _param;
      cfdDCS* worldDCS;
      cfdGroup* rootNode;
      cfdCommandArray* _commandArray;
      cfdReadParam* _readParam;
      cfdTeacher* _teacher;
      // cur_box will eventually be used to define bounding box
      // for data interagation
      double cur_box[6];
      vtkPolyData * arrow;
      float worldScale[ 3 ];
      float worldTrans[ 3 ];
      float worldRot[ 3 ];
};
#endif
