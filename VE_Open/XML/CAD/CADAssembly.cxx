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
 * File:          $RCSfile: CADAssembly.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Open/XML/CAD/CADAssembly.h"
#include "VE_Open/XML/CAD/CADPart.h"
#include "VE_Open/XML/CAD/CADClone.h"
#include <sstream>
XERCES_CPP_NAMESPACE_USE
using namespace VE_CAD;
////////////////////////////////////////////////////////////////////
CADAssembly::CADAssembly(std::string name)
:VE_CAD::CADNode(name)
{
  _numChildren = 0;
  _type = std::string("Assembly");
  SetObjectType("CADAssembly");
}
///////////////////////////
///Destructor            //
///////////////////////////
CADAssembly::~CADAssembly()
{
   for(unsigned int i = 0; i < _numChildren; i++)
   { 
      try
      {
         if(_children.at(i))
         {
        
            delete _children.at(i);
         }
        
         _children.at(i) = 0;
      } 
      catch(...)
      {
         std::cout<<"Child deleted!"<<std::endl;
      }
   }
   _children.clear();
   _numChildren = 0;
}
/////////////////////////////////////////////////
void CADAssembly::AddChild(VE_CAD::CADNode* node)
{
   _children.push_back(node);
   node->SetParent(_uID);
   _numChildren = static_cast< unsigned int >(_children.size());
}
////////////////////////////////////////////////////
bool CADAssembly::RemoveChild(VE_CAD::CADNode* node)
{
   /*for(unsigned int i = 0; i < _numChildren; i++)
   {
      if(node == _children.at(i))
      {
         //delete the whole vector and copy back only the valid children
         //this couldn't be more inefficient
         std::vector<CADNode> temp;

         for(unsigned int j = 0; j < _numChildren; j++)
         {
            if(i != j) 
               temp.push_back(_children.at(j));
         }
         //delete _children.at(i);
         _children.at(i) = 0;
         _children = temp;
         _numChildren = _children.size();
         return true;
      }
   }*/
   std::cout<<"CADAssembly::RemoveChild() not implemented yet!!!"<<std::endl;
   return false;
}
//////////////////////////////////////////////////////
bool CADAssembly::RemoveChild(unsigned int whichChild) 
{
   std::cout<<"CADAssembly::RemoveChild() not implemented yet!!!"<<std::endl;
   return false;
}
///////////////////////////////////////////////
unsigned int CADAssembly::GetNumberOfChildren()
{
   return _numChildren; 
}
/////////////////////////////////////////////////////////
VE_CAD::CADNode* CADAssembly::GetChild(std::string name)
{
   for(size_t i = 0; i < _numChildren; i++)
   {
      if(_children.at(i)->GetNodeName() == name)
      {
         return _children.at(i);
      }
   }
   return 0;
}
///////////////////////////////////////////////////////////////
VE_CAD::CADNode* CADAssembly::GetChild(unsigned int whichChild)
{
   return _children.at(whichChild);
}
///////////////////////////////////
void CADAssembly::_updateChildren()
{
   DOMElement* childList = _rootDocument->createElement(xercesString("children"));
   
   //the number of children
   DOMElement* nchildrenElement = _rootDocument->createElement(xercesString("numChildren"));
   std::stringstream int2string;
   int2string<<_numChildren;
   DOMText* numberOfChildren = _rootDocument->createTextNode(xercesString(int2string.str().c_str()));
   nchildrenElement->appendChild(numberOfChildren);
   _veElement->appendChild(nchildrenElement);

   //add the children nodes to the list
   for(unsigned int i = 0; i < _numChildren;  i++){
      _children.at(i)->SetOwnerDocument(_rootDocument);
      _children.at(i)->SetParent(_uID);
      childList->appendChild( _children.at( i )->GetXMLData("child") );
   }
   _veElement->appendChild(childList);
}
/////////////////////////////////////////////////////
void CADAssembly::_updateVEElement(std::string input)
{
   //Get the base elements from CADNode
   VE_CAD::CADNode::_updateVEElement(input);
   _updateChildren();
}
/////////////////////////////////////////////////////
void CADAssembly::SetObjectFromXMLData( DOMNode* xmlNode)
{
   DOMElement* currentElement = 0;

   if(xmlNode->getNodeType() == DOMNode::ELEMENT_NODE)
   {
      currentElement = dynamic_cast<DOMElement*>(xmlNode);
   }
   
   if(currentElement)
   {
      //populate the base elements in node
      VE_CAD::CADNode::SetObjectFromXMLData(xmlNode);

      //clear out the current list of children
      if(_numChildren){
         for(int i = _numChildren -1; i >=0; i--)
         {
           delete _children.at(i);
         }
         _children.clear();
      }
      //get the new number of children
      {
          DOMElement* nChildrenElement = GetSubElement(currentElement,std::string("numChildren"),0);
          _numChildren = ExtractIntegerDataNumberFromSimpleElement(nChildrenElement);
      }
      //populate the childList
      {
         DOMNodeList* childList = currentElement->getElementsByTagName(xercesString("children"));
         DOMElement* childListElement = dynamic_cast<DOMElement*>(childList->item(0));
         DOMNodeList* childrenNodes = childListElement->getElementsByTagName(xercesString("child"));
         for(unsigned int i = 0; i < _numChildren; i++)
         {
            DOMElement* cadNode = dynamic_cast<DOMElement*>(childrenNodes->item(i));
            DOMElement* nodeType = 0;
            unsigned int k = 0;
            while(!nodeType)
            {
               nodeType = GetSubElement(cadNode,std::string("type"),k);
               k++;
            }
            if(nodeType){
               if(ExtractDataStringFromSimpleElement(nodeType) == std::string("Assembly"))
               {
                  //this is an Assembly
                  VE_CAD::CADAssembly* newAssembly = new VE_CAD::CADAssembly();
                  newAssembly->SetObjectFromXMLData(cadNode);
                  newAssembly->SetParent(_uID);
                  _children.push_back(newAssembly);
               }else if(ExtractDataStringFromSimpleElement(nodeType) == std::string("Part")){
                  //this is a Part
                  VE_CAD::CADPart* newPart = new VE_CAD::CADPart();
                  newPart->SetObjectFromXMLData(cadNode);
                  newPart->SetParent(_uID);
                  _children.push_back(newPart);
               }else if(ExtractDataStringFromSimpleElement(nodeType) == std::string("Clone")){
                  //this is a Clone
                  VE_CAD::CADClone* newClone = new VE_CAD::CADClone();
                  newClone->SetObjectFromXMLData(cadNode);
                  newClone->SetParent(_uID);
                  _children.push_back(newClone);
               }else{
                  std::cout<<"ERROR!"<<std::endl;
                  std::cout<<"Unknown node type:"<<ExtractDataStringFromSimpleElement(nodeType)<<std::endl;    
               }
            }
         }
      }
   }
}
/////////////////////////////////////////////////
CADAssembly::CADAssembly(const CADAssembly& rhs)
:VE_CAD::CADNode(rhs)
{
   _numChildren = rhs._numChildren;
   for(unsigned int i = 0; i < _numChildren; i++){
      _children.push_back(rhs._children.at(i));
   }
}
///////////////////////////////////////////////////////////
CADAssembly& CADAssembly::operator=(const CADAssembly& rhs)
{
   if ( this != &rhs )
   {
      VE_CAD::CADNode::operator =(rhs);
      for(unsigned int i = _numChildren -1; i >= 0; i--)
      {
         delete _children.at(i);
      }
      _children.clear();
      _numChildren = rhs._numChildren;

      for(unsigned int i =0; i < _numChildren; i++)
      {
         _children.push_back(rhs._children.at(i));
      }
   }
   return *this;
}

