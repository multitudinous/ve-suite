#include "VE_XML/VECommand.h"
#include "VE_XML/VEDataValuePair.h"
#include <iostream>
using namespace VE_XML;
//////////////////////
//Constructor       //
//////////////////////
VECommand::VECommand(DOMDocument* rootDoc)
:VEXMLObject(rootDoc)
{
   _cmdName =  '\0';
   _nDataValuePairs = 0;
}
///////////////////////
VECommand::~VECommand()
{
   if(_nDataValuePairs)
   {
      for(unsigned int i = 0; i < _nDataValuePairs; i++)
      {
         delete _dataValuePairs.at(i);
      }
      _dataValuePairs.clear();
   }
}
///////////////////////////////////////////////////////////////////////////
void VECommand::AddDataValuePair(VE_XML::VEDataValuePair* commandValuePair)
{
   _dataValuePairs.push_back(commandValuePair);
   _nDataValuePairs = static_cast< unsigned int >( _dataValuePairs.size() );
}
/////////////////////////////////
void VECommand::_updateVEElement( std::string input )
{
   if(!_veElement)
   {
      _veElement = _rootDocument->createElement(xercesString("vecommand"));
   }
   //Be sure to set the number of children (_nChildren) either here or in the updating subElements code
   //_nChildren will be the number of dvPairs + the name of the command but we have to call the
   //update functions below to get the ndvPairs before we can calculate _nChildren

   //Add code here to update the specific sub elements
   _updateCommandName();
   _updateDataValuePairs();

   _nChildren = 1 + _nDataValuePairs;
}
////////////////////////////////////
void VECommand::_updateCommandName()
{
   DOMElement* cmdNameElement = _rootDocument->createElement(xercesString("name"));
   DOMText* cmdName = _rootDocument->createTextNode(xercesString(_cmdName.c_str()));
   cmdNameElement->appendChild(cmdName);
   _veElement->appendChild(cmdNameElement);
}
///////////////////////////////////////
void VECommand::_updateDataValuePairs()
{
   for(unsigned int i = 0; i < _nDataValuePairs;  i++){
      _veElement->appendChild( _dataValuePairs.at( i )->GetXMLData( "parameter" ) );
   }
}
/////////////////////////////////////////////////////////
//set the data from an string representing the xml     //
/////////////////////////////////////////////////////////
void VECommand::SetObjectFromXMLData(DOMNode* xmlInput)
{
   
   DOMElement* currentElement = 0;
   if(xmlInput->getNodeType() == DOMNode::ELEMENT_NODE){
      currentElement = dynamic_cast<DOMElement*>(xmlInput);
   }
   if(currentElement){
      //break down the element
      {
         //get variables by tags
         DOMNodeList* subElements = currentElement->getElementsByTagName(xercesString("command"));
      
         //should only be the name of the command
         DOMElement* name = dynamic_cast<DOMElement*>(subElements->item(0));
         if(name)
         {
            ExtractCmdNameFromElement(name);
         }
      }
      //break down the element
      {
         //get variables by tags
         DOMNodeList* subElements = currentElement->getElementsByTagName(xercesString("parameter"));
      

         //we can have as many dvpairs as we want so get them all and populate the list
         DOMElement* dataValuePairIn = 0;
         unsigned int nDVPairsIn = subElements->getLength();

         if( nDVPairsIn && _nDataValuePairs)
         {  
            //clear out old dvpairs
            for(unsigned int i = _nDataValuePairs -1; i > - 1;  i--){
               delete _dataValuePairs.at(i);
            }
            _dataValuePairs.clear();
         }
         //read in new data value pairs
         for(unsigned int i = 0; i < nDVPairsIn; i++){
            DOMElement* dvPairIn = dynamic_cast<DOMElement*>(subElements->item(i));
            if(dvPairIn)
            {
               VE_XML::VEDataValuePair* veDvp = new VE_XML::VEDataValuePair(_rootDocument);
               veDvp->SetObjectFromXMLData(dvPairIn);
               _dataValuePairs.push_back(veDvp);
            }
         }
      }
   }
   _nDataValuePairs = static_cast< unsigned int >( _dataValuePairs.size() );
   _nChildren = 1 + _nDataValuePairs;
}
/////////////////////////////////////////////////////////////////////
void VECommand::ExtractCmdNameFromElement(DOMElement* commandElement)
{
   _cmdName = std::string(XMLString::transcode(commandElement->getNodeValue()));
}
///////////////////////////////////////
std::string VECommand::GetCommandName()
{
   return _cmdName;
}
///////////////////////////////////////
void VECommand::SetCommandName( std::string name )
{
   _cmdName = name;
}
//////////////////////////////////////////////////////////////////////////////
VE_XML::VEDataValuePair* VECommand::GetDataValuePair(std::string dataValueName)
{
   for(unsigned int i = 0; i < _nDataValuePairs; i++)
   {
      if(dataValueName == _dataValuePairs.at(i)->GetDataName())
      {
         return _dataValuePairs.at(i);
      }
   }
   return 0;
}
////////////////////////////////////////////////////////////////////////
VE_XML::VEDataValuePair* VECommand::GetDataValuePair(unsigned int index)
{
   if(_nDataValuePairs)
   {
      if(_dataValuePairs.at(index))
      {
         return _dataValuePairs.at(index);
      }
   }
   std::cout<<"Invalid index: "<< index<<" specified for VECommand::GetDataValuePair()"<<std::endl;
   return 0;
}
///////////////////////////////////////////////////
unsigned int VECommand::GetNumberOfDataValuePairs()
{
   return _nDataValuePairs;
}
   
