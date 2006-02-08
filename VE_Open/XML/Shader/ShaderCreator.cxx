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
 * File:          $RCSfile: SceneGraphBuilder.cxx,v $
 * Date modified: $Date: 2006-01-10 13:45:28 -0600 (Tue, 10 Jan 2006) $
 * Version:       $Rev: 3477 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Open/XML/Shader/ShaderCreator.h"
#include "VE_Open/XML/Shader/TextureImage.h"
#include "VE_Open/XML/Shader/Shader.h"
#include "VE_Open/XML/Shader/Uniform.h"
#include "VE_Open/XML/Shader/Program.h"

using namespace VE_Shader;
//////////////////////////////////////////////////////////////////////
VE_XML::XMLObject* ShaderCreator::CreateNewXMLObject(std::string objectType)
{
   if(objectType == "Program")
   {
      return new Program();
   }
   else if(objectType == "Shader")
   {
      return new Shader();
   } 
   else if(objectType == "Uniform")
   {
      return new Uniform();
   }
   else if(objectType == "TextureImage")
   {
      return new TextureImage();
   }
   return 0;
}
//////////////////////////////////////////////////////////////////////
VE_XML::XMLObject* ShaderCreator::CreateNewXMLObjectCopy(std::string objectType,
                                                     VE_XML::XMLObject* objectToCopy)
{
   if(objectType == "Program"){
      return new Program(*dynamic_cast<Program*>(objectToCopy));
   }else if(objectType == "Shader"){
      return new Shader(*dynamic_cast<Shader*>(objectToCopy));
   }else if(objectType == "Uniform"){
      return new Uniform(*dynamic_cast<Uniform*>(objectToCopy));
   }else if(objectType == "TextureImage"){
      return new TextureImage(*dynamic_cast<TextureImage*>(objectToCopy));
   }
   return 0;
}
