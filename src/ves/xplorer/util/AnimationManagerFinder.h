struct AnimationManagerFinder : public osg::NodeVisitor
{
    AnimationManagerFinder()
        :
        osg::NodeVisitor(osg::NodeVisitor::TRAVERSE_ALL_CHILDREN) 
    {
        ;
    }

    void apply(osg::Node& node) 
    {
        if (_am.valid())
            return;
        if (node.getUpdateCallback()) 
        {
            osgAnimation::AnimationManagerBase* b = dynamic_cast<osgAnimation::AnimationManagerBase*>(node.getUpdateCallback());
            if (b) 
            {
                _am = new osgAnimation::BasicAnimationManager(*b);
                return;
            }
        }
        traverse(node);
    }
    
    osg::ref_ptr<osgAnimation::BasicAnimationManager> _am;
};


