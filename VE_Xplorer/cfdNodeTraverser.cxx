
#include "cfdNodeTraverser.h"
#include <iostream>
using namespace std;
#include <Performer/pf/pfGroup.h>
////////////////////////////////////
//Constructors                    //
////////////////////////////////////
cfdNodeTraverser::cfdNodeTraverser()
{ 
   _root = 0;
   _preFunc = 0;
   _postFunc = 0;
}
/////////////////////////////////////////////////////////////////
cfdNodeTraverser::cfdNodeTraverser(const cfdNodeTraverser& cfdNT)
{
   _root = 0;
   _preFunc = 0;
   _postFunc = 0;

   _root = cfdNT._root;
   _preFunc = cfdNT._preFunc;
   _postFunc = cfdNT._postFunc;
}
/////////////////////////////////////
//Destructor                       //
/////////////////////////////////////
cfdNodeTraverser::~cfdNodeTraverser()
{
   
}
////////////////////////////////////////////
//set the node to traverse                //
////////////////////////////////////////////
void cfdNodeTraverser::setNode(pfNode* root)
{
   _root = root;
}
/////////////////////////////////
//traverse the node            //
/////////////////////////////////
void cfdNodeTraverser::traverse()
{
   if(_root){
      //the pre-callback
      if(_preFunc){
         _preFunc(this,_root);
      }

      //recurse the root node
      _traverseNode(_root);

      //the post-callback
      if(_postFunc){
         _postFunc(this,_root);
      }
   }else{
      cout<<"Error: cfdNodeTraverser::traverse()!"<<endl;
      cout<<"Root node not set!"<<endl;
      return; 
   }
}
/////////////////////////////////////////////////////
//depth first recursion of a node/scene graph      //
/////////////////////////////////////////////////////
void cfdNodeTraverser::_traverseNode(pfNode* node)
{
   int nChildren = 0;
   
   if(!node->isOfType(pfGroup::getClassType())){
      return;
   }
   //grab the children of this group
   pfGroup* curGroup = (pfGroup*)node;
   nChildren = curGroup->getNumChildren();

   //the pre-callback
   if(_preFunc){
      _preFunc(this,node);
   }

   //recurse the children of this node
   for(int i = 0; i < nChildren; i++){
      _traverseNode(curGroup->getChild(i));
   }

   //the post-callback
   if(_postFunc){
      _postFunc(this,node);
   }
}

//////////////////////////////////////////////////////////
//the equal operator                                    //
//////////////////////////////////////////////////////////
cfdNodeTraverser&
cfdNodeTraverser::operator=(const cfdNodeTraverser& cfdNT)
{
   if(this!= &cfdNT){
      _root = cfdNT._root;
      _preFunc = cfdNT._preFunc;
      _postFunc = cfdNT._postFunc;
   }
   return *this;
   
}
