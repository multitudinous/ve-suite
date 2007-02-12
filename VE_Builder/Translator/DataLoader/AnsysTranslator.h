/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * Date modified: $Date: 2006-07-20 21:15:37 -0500 (Thu, 20 Jul 2006) $
 * Version:       $Rev: 4995 $
 * Author:        $Author: mccdo $
 * Id:            $Id: StarCDTranslator.h 4995 2006-07-21 02:15:37Z mccdo $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef ANSYS_TRANSLATOR_H_
#define ANSYS_TRANSLATOR_H_


#include "VE_Builder/Translator/cfdTranslatorToVTK/cfdTranslatorToVTK.h"

namespace VE_Builder
{
class VE_USER_BUILDER_EXPORTS AnsysTranslator : public VE_Builder::cfdTranslatorToVTK
{
public:
   AnsysTranslator();
   virtual ~AnsysTranslator();
   ///Display help for the StarCD translator
   virtual void DisplayHelp( void );
   
   class VE_USER_BUILDER_EXPORTS AnsysTranslateCbk: public VE_Builder::cfdTranslatorToVTK::TranslateCallback
   {
   public:
      AnsysTranslateCbk(){};
      virtual ~AnsysTranslateCbk(){};
      //////////////////////////////////////////////////
      //ouputDataset should be populated              //
      //appropriately by the translate callback.      //
      //////////////////////////////////////////////////
      virtual void Translate(vtkDataObject*& outputDataset,
		                     cfdTranslatorToVTK* toVTK);
   protected:
   };
   class VE_USER_BUILDER_EXPORTS AnsysPreTranslateCbk: public VE_Builder::cfdTranslatorToVTK::PreTranslateCallback{
   public:
      AnsysPreTranslateCbk(){};
      virtual ~AnsysPreTranslateCbk(){};
      void Preprocess(int argc,char** argv,VE_Builder::cfdTranslatorToVTK* toVTK);
   protected:
   };
protected:
   AnsysPreTranslateCbk _cmdParser;
   AnsysTranslateCbk ansysToVTK;
};

}
#endif//ANSYS_TRANSLATOR_H_
