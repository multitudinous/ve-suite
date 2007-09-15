#ifndef GETVESUITE_MultiFrac_MultiFrac_PREFLASH_H
#define GETVESUITE_MultiFrac_MultiFrac_PREFLASH_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_MultiFrac_MultiFrac_PREFLASH( void )
{
    unsigned char osgData[ 8952 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,167,0,138,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,254,254,40,162,138,0,40,162,138,0,40,162,188,127,227,191,199,127,134,127,179,95,195,61,83,226,239,197,221,83,196,26,79,130,116,159,16,120,3,194,114,73,225,63,0,124,66,248,169,226,173,75,197,95,21,62,33,120,91,225,71,195,191,12,248,103,225,223,194,143,11,107,126,34,241,127,136,53,127,136,158,54,240,174,149,101,101,165,105,87,183,50,92,235,49,147,24,136,73,34,0,123,5,21,240,7,252,60,179,246,117,255,0,162,115,251,127,255,0,226,167,127,224,169,191,253,6,244,127,195,203,63,103,95,250,39,63,183,255,0,254,42,119,254,10,155,255,0,208,111,64,31,127,215,201,63,182,247,237,119,224,191,216,135,246,116,241,143,199,175,23,105,79,226,187,221,42,125,39,195,158,2,248,119,99,175,104,254,31,215,62,37,252,67,241,53,226,216,120,115,194,58,45,230,175,33,34,53,95,183,106,154,188,214,150,186,149,238,153,225,223,13,107,90,204,58,94,162,52,199,180,151,205,191,225,229,159,179,175,253,19,159,219,255,0,255,0,21,59,255,0,5,77,255,0,232,55,175,231,127,254,10,247,251,76,248,7,246,191,253,170,255,0,101,207,1,248,67,193,63,22,60,49,163,252,16,248,109,227,127,27,62,179,241,207,224,15,199,159,217,251,197,26,246,177,241,175,197,154,39,135,36,176,210,254,21,254,210,31,10,60,35,173,174,139,165,90,124,25,182,154,31,17,90,67,125,166,106,183,30,42,188,211,32,150,218,239,66,191,89,113,196,85,246,52,103,83,172,86,158,175,68,52,174,210,62,109,248,163,227,159,219,103,254,10,61,173,234,58,199,237,3,241,83,196,154,87,129,60,71,165,232,154,38,165,251,59,124,37,214,60,97,224,223,217,244,105,222,23,198,181,166,139,207,135,83,248,178,254,47,21,107,73,227,11,52,214,102,213,53,203,189,90,252,95,53,180,22,215,86,214,90,94,151,167,105,220,159,132,244,143,218,95,254,9,201,113,99,241,63,224,31,198,239,23,252,48,240,87,128,175,238,190,38,120,147,225,167,138,60,91,174,15,217,219,196,243,67,166,75,167,248,195,84,248,173,240,238,93,126,215,71,213,116,235,191,7,105,54,214,55,218,156,141,105,169,105,214,250,125,174,161,97,171,105,119,154,101,134,163,103,251,183,251,30,124,16,208,175,52,45,42,41,35,181,219,36,81,200,38,82,240,114,169,28,178,179,65,38,228,141,120,8,19,205,17,108,224,236,92,129,243,239,196,15,133,26,63,237,107,225,79,11,254,209,51,173,189,191,236,197,173,105,62,29,248,153,251,47,248,22,73,102,75,191,138,90,13,245,189,151,136,124,9,251,79,252,91,211,161,0,217,71,117,105,38,149,173,124,55,240,45,195,69,63,134,173,231,179,241,159,142,96,139,226,77,198,143,225,111,131,255,0,31,28,206,188,235,186,142,171,80,167,163,126,187,37,230,236,254,230,217,187,130,229,74,215,111,95,150,135,184,255,0,193,49,254,54,120,219,254,10,157,241,83,195,63,183,55,196,143,136,31,24,127,102,175,27,126,201,144,201,240,91,87,253,140,254,22,252,120,248,143,165,252,33,248,232,60,89,240,227,196,47,39,237,13,251,74,126,206,222,47,213,102,177,210,252,45,169,107,62,60,214,219,225,190,138,186,30,155,226,159,14,234,159,8,181,19,227,15,30,120,234,123,61,47,195,95,15,63,161,58,254,53,127,224,151,186,237,151,193,111,248,43,119,131,52,91,77,14,215,87,127,143,255,0,10,254,52,124,17,154,250,221,161,208,159,195,75,163,105,22,63,31,224,241,36,177,69,103,112,124,64,184,248,42,218,58,89,51,217,136,199,138,77,250,221,159,177,253,138,243,251,42,175,173,194,87,250,197,8,85,234,247,245,48,106,205,174,193,69,20,87,72,130,138,40,160,2,138,40,160,2,138,40,160,2,190,0,255,0,130,150,127,201,186,252,57,255,0,179,255,0,255,0,130,78,255,0,235,211,127,99,122,251,254,191,41,191,224,171,31,31,190,4,248,7,225,55,195,111,135,126,58,248,213,240,147,193,127,16,36,253,177,255,0,224,152,95,19,163,240,47,139,126,35,248,59,195,158,49,147,225,175,131,63,224,166,63,179,7,138,60,97,241,13,60,49,172,107,48,222,191,129,180,175,13,120,23,198,218,142,165,171,8,62,193,99,99,224,237,86,238,234,226,43,125,62,238,72,64,63,86,104,174,59,192,31,17,62,31,252,87,240,150,147,227,255,0,133,190,58,240,119,196,175,2,107,255,0,111,254,194,241,175,128,60,79,162,120,199,194,90,215,246,94,167,123,162,234,127,217,62,36,240,237,245,205,158,163,246,109,103,78,212,45,39,242,102,127,38,230,194,104,36,219,44,78,139,216,210,186,123,107,232,1,95,205,239,252,28,11,240,51,197,177,105,191,179,255,0,237,159,225,29,3,195,3,194,223,3,159,196,191,14,254,63,120,142,223,80,180,208,188,122,60,41,241,63,197,63,15,180,223,131,183,178,201,246,5,151,198,62,14,209,254,33,79,226,59,52,178,23,111,113,165,95,124,90,55,246,118,38,198,231,95,212,44,127,164,42,249,255,0,227,167,192,191,248,95,255,0,240,139,248,43,198,190,41,255,0,139,1,255,0,19,187,175,140,191,6,237,116,79,249,47,255,0,242,8,79,9,124,61,248,131,226,215,213,255,0,228,223,255,0,228,100,159,198,62,14,135,77,255,0,139,129,255,0,18,125,3,95,214,63,225,95,255,0,194,111,224,143,136,17,86,154,171,78,84,229,180,144,211,179,79,177,252,138,126,207,63,180,54,155,251,76,105,86,190,26,241,30,161,4,31,178,220,24,135,196,58,68,239,231,191,237,88,214,201,47,252,83,254,34,154,119,103,255,0,134,87,242,237,230,138,250,208,128,62,42,178,29,63,81,142,63,133,169,127,166,252,84,251,187,246,152,253,175,116,187,239,15,223,200,250,157,180,242,201,108,102,137,208,169,16,182,215,84,6,227,206,141,129,140,65,59,130,25,2,12,23,96,171,186,186,15,218,63,254,8,1,168,105,254,33,213,252,109,251,2,252,115,176,248,61,166,13,47,64,93,19,246,121,248,189,31,141,252,93,224,68,241,2,73,14,141,226,91,251,47,141,210,235,250,207,136,252,55,160,203,225,242,250,140,58,125,230,139,226,105,191,182,226,185,134,61,70,199,73,212,45,160,209,62,103,255,0,130,125,127,193,28,254,35,254,216,63,8,127,102,63,218,211,246,181,248,253,166,91,124,40,248,205,240,247,225,103,198,232,255,0,103,79,132,246,62,36,184,212,60,85,240,195,226,31,195,223,14,252,76,240,151,135,252,89,241,162,254,247,68,187,240,70,185,38,163,226,83,97,226,11,77,27,67,189,115,166,233,147,38,141,226,107,59,237,70,43,253,39,231,42,100,213,39,90,54,74,52,161,178,190,157,47,234,223,87,109,149,182,178,53,85,18,77,61,91,255,0,129,167,161,218,127,193,13,254,15,248,171,227,55,237,109,241,99,246,201,191,209,124,47,171,252,27,248,83,225,31,25,252,16,240,111,136,53,205,86,211,94,241,36,63,31,60,83,123,240,243,196,154,206,173,224,125,38,109,62,73,116,22,210,254,17,222,106,58,118,161,173,25,108,164,184,179,248,176,154,86,156,215,240,203,226,24,116,239,235,6,188,223,225,15,194,31,134,159,1,62,26,120,59,224,247,193,239,7,105,30,2,248,109,224,45,33,52,95,10,248,87,69,73,254,201,97,105,231,205,121,119,115,115,119,121,52,183,58,198,181,121,169,221,94,222,234,90,149,236,247,58,142,169,168,234,55,90,142,163,117,117,125,117,113,113,39,164,87,208,208,163,26,20,163,74,59,71,243,50,110,238,225,69,20,86,194,10,40,162,128,10,40,162,128,10,252,38,255,0,130,174,127,193,101,180,143,216,155,90,211,63,103,175,217,207,70,240,127,198,15,218,235,83,155,65,213,124,79,162,120,152,106,154,167,195,143,130,30,13,190,22,154,196,87,31,19,32,240,214,185,167,94,106,94,62,215,52,7,13,162,120,118,219,82,176,158,207,78,212,83,197,122,229,205,166,151,253,133,97,226,191,218,63,137,126,53,255,0,133,109,240,231,199,255,0,17,127,225,18,241,183,143,191,225,1,240,79,138,188,107,255,0,8,39,195,77,7,254,18,175,136,222,53,255,0,132,87,66,191,215,127,225,18,240,7,134,62,213,7,252,36,126,54,212,190,193,246,61,42,195,207,135,237,151,247,144,91,249,177,249,155,199,249,164,254,203,119,154,183,198,239,23,248,151,227,31,143,60,67,161,120,227,199,127,23,124,93,226,47,138,158,59,241,45,155,216,219,219,120,163,197,254,62,215,174,252,91,226,111,17,197,165,120,126,198,223,78,176,130,247,93,213,239,46,86,11,27,107,123,40,69,210,199,107,12,48,172,104,188,152,204,67,195,210,230,74,237,149,21,119,99,234,207,18,91,126,216,127,182,205,195,106,255,0,181,95,237,5,241,79,227,38,155,127,170,232,190,38,151,193,250,222,188,250,55,194,157,63,196,26,39,133,207,131,172,53,207,12,124,31,240,212,26,119,134,60,41,173,38,142,247,76,110,52,157,35,77,184,185,109,99,83,158,225,229,185,191,212,37,184,245,191,6,255,0,193,55,52,11,77,38,88,223,67,143,121,191,153,216,181,164,7,113,242,109,128,43,246,241,230,32,218,160,96,22,92,169,32,228,176,31,170,63,179,71,195,45,10,91,29,61,238,19,78,138,86,85,68,141,39,194,162,156,35,179,71,36,64,135,127,157,73,12,8,141,128,70,77,204,79,235,183,195,207,128,250,5,245,175,218,82,198,4,155,236,155,101,99,28,63,189,217,178,76,186,252,224,159,148,243,142,49,146,112,205,95,25,138,204,235,115,52,234,61,247,185,209,24,117,217,31,199,135,196,143,248,39,22,137,107,115,170,94,193,162,197,20,178,199,12,16,74,208,44,72,151,70,197,54,76,172,191,232,241,170,36,101,136,219,32,38,35,184,49,98,163,214,62,17,255,0,193,78,127,224,164,31,176,103,136,238,46,245,111,137,222,33,253,169,254,22,221,235,45,171,120,159,225,135,237,37,226,29,127,198,250,236,150,211,223,120,66,61,98,231,193,127,24,181,43,171,143,17,248,27,88,255,0,132,95,194,119,182,118,17,77,115,171,248,91,75,185,241,109,246,177,115,224,237,74,241,167,156,127,73,95,22,254,12,248,115,74,180,186,132,90,91,64,138,179,9,90,79,37,86,119,112,193,149,156,179,249,140,202,206,191,50,144,84,179,97,91,4,126,18,126,213,159,13,116,152,151,80,142,11,125,54,225,2,73,36,82,52,211,60,136,223,49,80,210,199,16,249,213,144,224,231,118,221,172,204,91,38,186,48,89,157,103,40,251,205,173,55,249,19,40,90,215,91,159,214,111,236,143,251,92,124,17,253,183,62,8,248,115,227,215,192,95,18,73,173,120,79,90,150,109,39,92,208,181,104,109,244,239,26,252,57,241,182,157,111,103,63,136,126,28,252,71,240,244,23,147,255,0,194,57,227,109,55,237,214,109,52,43,53,197,165,229,166,161,101,171,233,23,186,150,135,169,105,154,157,239,211,53,252,5,255,0,193,24,255,0,109,31,10,126,196,63,240,80,47,25,124,61,241,180,223,18,117,175,135,63,181,111,195,173,83,195,3,193,255,0,3,190,13,120,231,246,137,241,158,163,241,123,225,44,122,191,196,127,1,248,150,235,192,159,9,252,31,174,248,222,231,195,250,119,195,136,190,58,90,207,255,0,8,222,155,168,68,179,248,182,214,247,91,176,26,109,131,107,26,47,245,235,255,0,15,44,253,157,127,232,156,254,223,255,0,248,169,223,248,42,111,255,0,65,189,125,141,10,158,214,148,39,107,115,35,6,172,218,236,125,255,0,95,0,127,193,39,127,229,22,95,240,77,63,251,48,15,216,223,255,0,89,215,225,205,31,240,242,207,217,215,254,137,207,237,255,0,255,0,138,157,255,0,130,166,255,0,244,27,215,160,127,193,61,190,22,248,239,224,119,236,11,251,15,124,20,248,165,161,127,194,47,241,55,224,255,0,236,129,251,52,252,45,248,139,225,159,237,61,27,91,255,0,132,119,199,127,15,254,12,120,43,194,126,46,208,191,182,124,59,168,93,233,250,191,217,60,65,164,106,22,255,0,106,177,187,185,179,184,251,63,155,107,113,52,14,146,54,162,62,191,162,138,40,0,162,138,40,0,162,138,40,0,162,138,40,0,175,242,213,255,0,130,93,156,248,3,225,225,245,240,31,135,15,254,83,52,154,255,0,82,170,255,0,45,95,248,37,215,252,147,255,0,135,127,246,33,248,111,255,0,77,154,77,121,153,175,251,186,245,46,27,191,67,250,201,253,156,191,213,88,125,19,249,173,126,229,252,39,255,0,144,123,255,0,215,172,255,0,250,34,74,252,52,253,156,191,213,88,125,19,249,173,126,229,252,39,255,0,144,123,255,0,215,172,255,0,250,34,74,252,255,0,21,188,189,127,200,234,134,207,212,249,167,246,133,255,0,143,59,207,250,236,127,244,92,213,248,11,251,78,255,0,170,191,255,0,118,79,253,6,74,253,250,253,161,127,227,206,243,254,187,31,253,23,53,126,2,254,211,191,234,175,255,0,221,147,255,0,65,146,186,48,59,175,151,232,42,157,15,201,79,248,37,31,252,167,131,246,1,255,0,177,151,246,153,255,0,214,42,253,165,107,253,36,107,252,219,191,224,148,127,242,158,15,216,7,254,198,95,218,103,255,0,88,171,246,149,175,244,145,175,208,176,159,238,244,253,14,89,252,76,40,162,138,233,36,40,162,138,0,40,162,138,0,40,162,138,0,40,175,31,248,215,251,66,252,2,253,154,252,43,167,248,235,246,140,248,227,240,127,224,15,130,117,111,16,90,248,79,75,241,143,198,191,137,126,11,248,87,225,93,75,197,87,218,118,171,172,88,248,103,79,241,15,142,181,171,11,75,207,16,77,164,104,90,229,212,86,81,204,215,50,91,104,215,115,164,102,43,121,153,62,96,255,0,135,176,255,0,193,44,191,233,37,159,176,7,254,38,71,236,235,255,0,207,26,128,62,255,0,175,242,213,255,0,130,93,127,201,63,248,119,255,0,98,31,134,255,0,244,217,164,215,247,89,241,207,254,11,65,251,2,252,41,255,0,132,91,226,47,133,191,111,127,216,3,226,111,194,95,15,255,0,109,197,241,211,193,30,15,253,169,126,12,120,131,227,182,157,225,203,223,236,137,52,143,138,223,7,116,125,19,226,156,255,0,240,179,127,225,24,251,22,182,254,33,248,127,103,164,79,226,175,20,104,222,32,151,81,240,45,238,161,226,223,12,105,127,13,254,37,255,0,10,127,240,75,175,249,39,255,0,14,255,0,236,67,240,223,254,155,52,154,243,51,95,247,117,234,92,55,126,135,245,147,251,57,127,170,176,250,39,243,90,253,203,248,79,255,0,32,247,255,0,175,89,255,0,244,68,149,248,105,251,57,127,170,176,250,39,243,90,253,203,248,79,255,0,32,247,255,0,175,89,255,0,244,68,149,249,254,43,121,122,255,0,145,213,13,159,169,243,79,237,11,255,0,30,119,159,245,216,255,0,232,185,171,240,23,246,157,255,0,85,127,254,236,159,250,12,149,251,245,251,66,255,0,199,157,231,253,118,63,250,46,106,252,5,253,167,127,213,95,255,0,187,39,254,131,37,116,96,55,94,171,244,21,78,135,228,167,252,18,143,254,83,193,251,0,255,0,216,203,251,76,255,0,235,21,126,210,181,254,146,53,254,109,223,240,74,63,249,79,7,236,3,255,0,99,47,237,51,255,0,172,85,251,74,215,250,72,215,232,88,79,247,122,126,135,44,254,38,20,81,69,116,146,20,81,69,0,20,81,69,0,20,81,69,0,126,28,126,217,31,182,54,129,171,252,103,248,39,227,239,6,248,11,196,186,239,195,15,248,39,39,237,49,241,71,227,55,237,9,241,31,90,212,180,15,5,120,102,109,55,195,223,178,239,237,59,251,51,124,64,209,188,27,117,175,222,170,180,222,30,111,141,94,56,213,53,11,255,0,16,55,135,116,183,159,225,92,54,118,183,147,232,250,240,241,54,147,247,7,252,19,163,246,172,248,167,251,108,126,203,126,22,253,164,62,42,252,5,209,127,103,11,223,29,120,155,199,86,94,20,248,109,167,124,82,241,23,197,61,106,47,8,120,51,197,26,143,130,33,215,252,103,117,226,127,130,254,6,186,240,103,137,175,252,69,225,239,18,74,186,4,186,93,197,205,134,152,52,217,53,59,155,61,102,235,81,208,116,111,194,143,1,120,115,227,167,194,189,111,225,231,128,62,37,254,206,159,20,111,62,3,248,163,226,214,171,162,255,0,193,57,126,23,248,155,74,211,172,188,69,241,243,197,30,25,143,92,248,137,240,143,226,199,237,57,225,187,201,109,175,60,11,241,59,72,248,115,224,187,31,21,248,115,192,94,52,211,60,31,7,130,174,188,39,174,124,77,241,78,143,15,142,60,27,162,47,193,47,222,223,216,75,224,39,138,190,2,124,22,212,109,254,32,193,30,157,241,43,226,167,196,15,17,252,93,248,129,160,89,234,118,90,182,147,225,109,99,95,177,209,60,57,161,248,103,78,185,177,136,199,246,155,63,2,120,71,194,17,234,194,59,205,82,212,235,223,218,210,105,218,157,222,150,246,44,190,110,26,182,50,166,34,181,58,244,185,105,198,237,59,89,45,172,147,222,77,167,118,246,233,220,169,40,164,154,149,219,61,71,227,167,192,191,248,95,255,0,240,139,120,43,198,190,41,255,0,140,127,255,0,137,221,215,198,95,131,86,218,39,252,156,7,252,130,19,194,95,15,126,33,120,181,245,126,127,103,255,0,249,25,38,241,143,131,161,211,127,226,224,127,196,159,64,215,245,143,248,87,255,0,240,155,248,35,226,7,249,170,255,0,193,46,191,228,159,252,59,255,0,177,15,195,127,250,108,210,107,253,74,171,252,181,127,224,151,95,242,79,254,29,255,0,216,135,225,191,253,54,105,52,179,95,247,117,234,56,110,253,15,235,39,246,114,255,0,85,97,244,79,230,181,251,151,240,159,254,65,239,255,0,94,179,255,0,232,137,43,240,211,246,114,255,0,85,97,244,79,230,181,251,151,240,159,254,65,239,255,0,94,179,255,0,232,137,43,243,252,86,242,245,255,0,35,170,27,63,83,230,159,218,23,254,60,239,63,235,177,255,0,209,115,87,224,47,237,59,254,170,255,0,253,217,63,244,25,43,247,235,246,133,255,0,143,59,207,250,236,127,244,92,213,248,11,251,78,255,0,170,191,255,0,118,79,253,6,74,232,192,110,189,87,232,42,157,15,197,79,248,39,175,197,143,133,159,3,191,224,180,191,176,247,197,47,141,127,18,254,31,252,31,248,101,225,127,18,254,208,191,240,147,124,69,248,165,227,47,14,124,63,240,39,135,127,182,255,0,100,47,218,23,195,186,55,246,239,139,188,89,169,90,105,250,71,218,252,65,171,105,86,54,191,104,184,143,237,23,154,157,189,172,91,231,154,52,111,239,75,254,30,197,255,0,4,178,255,0,164,150,126,192,31,248,153,31,179,175,255,0,60,106,254,27,127,224,148,121,255,0,135,240,126,192,57,61,60,75,251,76,123,255,0,205,148,254,210,189,190,149,254,146,53,250,22,19,253,222,159,161,203,63,137,159,0,127,195,216,191,224,150,95,244,146,207,216,3,255,0,19,35,246,117,255,0,231,141,71,252,61,139,254,9,101,255,0,73,44,253,128,63,241,50,63,103,95,254,120,213,247,253,21,210,73,240,7,252,61,139,254,9,101,255,0,73,44,253,128,63,241,50,63,103,95,254,120,212,127,195,216,191,224,150,95,244,146,207,216,3,255,0,19,35,246,117,255,0,231,141,95,127,209,64,31,0,127,195,216,191,224,150,95,244,146,207,216,3,255,0,19,35,246,117,255,0,231,141,93,7,132,255,0,224,166,191,240,77,207,30,248,171,195,62,5,240,47,252,20,27,246,32,241,167,141,188,105,226,13,27,194,126,14,240,119,132,255,0,106,255,0,128,222,35,241,87,139,60,85,226,61,70,219,71,240,247,134,188,51,225,237,31,199,211,93,235,222,32,191,213,239,45,45,108,172,173,97,150,230,234,230,234,56,32,141,229,117,83,232,159,26,255,0,108,175,217,139,246,120,241,54,137,224,127,139,127,24,60,57,225,223,31,120,133,52,27,189,55,225,214,151,111,173,120,223,226,58,232,126,39,184,241,93,158,131,227,29,79,225,239,129,52,173,79,90,208,190,30,221,106,126,7,241,93,148,126,35,190,177,183,208,134,161,162,75,167,182,162,183,165,32,127,3,253,169,53,187,47,218,179,224,71,130,244,127,128,241,234,126,53,212,180,127,218,247,254,9,239,241,87,81,182,188,209,53,159,5,53,191,128,190,7,126,223,31,179,63,198,239,138,58,232,127,31,88,105,107,63,246,103,195,31,0,120,179,82,22,177,179,222,223,54,146,44,116,219,107,205,74,226,214,210,108,106,98,104,82,146,141,90,209,166,223,118,151,245,243,26,77,236,174,126,139,209,95,38,124,86,253,184,191,102,111,130,80,234,19,252,75,241,207,136,116,40,180,184,204,183,134,203,225,47,198,63,20,58,168,104,212,139,120,188,37,224,11,246,190,124,202,167,108,11,35,109,12,216,218,142,87,207,175,255,0,224,170,127,240,76,13,42,250,247,75,213,63,224,163,255,0,176,94,155,169,233,183,119,54,26,142,157,127,251,96,126,207,118,119,214,23,214,115,61,189,221,149,237,165,199,196,53,146,214,238,43,136,228,142,72,228,85,116,120,217,89,67,2,42,225,86,157,69,122,117,35,81,47,229,105,254,64,211,91,171,29,175,237,109,251,61,124,84,248,227,117,251,53,248,187,224,167,197,191,135,255,0,7,254,38,254,204,255,0,31,245,15,142,126,26,214,126,41,124,25,241,23,199,95,2,120,139,251,111,246,113,253,161,191,102,253,103,194,218,239,130,60,39,241,191,225,238,161,30,239,15,254,208,122,174,163,107,125,111,226,56,254,207,121,225,203,116,150,206,234,9,164,85,243,255,0,248,87,63,240,84,223,250,60,143,216,3,255,0,21,167,251,69,127,244,216,171,239,250,42,196,124,1,255,0,10,231,254,10,155,255,0,71,145,251,0,127,226,180,255,0,104,175,254,155,21,127,158,167,252,18,235,254,73,255,0,195,191,251,16,252,55,255,0,166,205,38,191,212,170,191,203,87,254,9,117,255,0,36,255,0,225,223,253,136,126,27,255,0,211,102,147,94,102,107,254,238,189,75,134,239,208,254,178,127,103,47,245,86,31,68,254,107,95,185,127,9,255,0,228,30,255,0,245,235,63,254,136,146,191,13,63,103,47,245,86,31,68,254,107,95,185,127,9,255,0,228,30,255,0,245,235,63,254,136,146,191,63,197,111,47,95,242,58,161,179,245,62,105,253,161,127,227,206,243,254,187,31,253,23,53,126,2,254,211,191,234,175,255,0,221,147,255,0,65,146,191,126,191,104,95,248,243,188,255,0,174,199,255,0,69,205,95,128,191,180,239,250,171,255,0,247,100,255,0,208,100,174,140,6,235,213,126,130,169,208,252,85,255,0,130,122,248,55,196,95,16,63,224,180,191,176,247,132,124,39,241,99,226,7,192,223,16,106,254,37,253,161,127,179,254,40,252,45,211,190,21,234,254,58,240,191,216,63,100,47,218,23,84,187,254,194,211,254,53,252,52,241,135,134,110,62,221,101,101,115,167,93,127,105,248,115,81,217,103,171,92,73,103,246,77,65,109,47,173,127,189,31,248,99,127,218,43,254,146,197,251,127,255,0,225,185,255,0,130,89,127,244,180,235,248,109,255,0,130,81,255,0,202,120,63,96,31,251,25,127,105,159,253,98,175,218,86,191,210,70,191,66,194,127,187,211,244,57,103,241,51,224,15,248,99,127,218,43,254,146,197,251,127,255,0,225,185,255,0,130,89,127,244,180,232,255,0,134,55,253,162,191,233,44,95,183,255,0,254,27,159,248,37,151,255,0,75,78,190,255,0,162,186,73,62,0,255,0,134,55,253,162,191,233,44,95,183,255,0,254,27,159,248,37,151,255,0,75,78,143,248,99,127,218,43,254,146,197,251,127,255,0,225,185,255,0,130,89,127,244,180,235,239,250,40,3,248,101,255,0,130,151,232,63,23,255,0,103,191,248,43,230,135,167,234,190,51,253,162,127,110,173,79,81,253,132,127,103,205,74,77,127,198,71,246,67,240,111,197,63,15,218,220,126,208,223,181,221,174,151,225,205,39,74,240,7,195,223,131,254,15,191,240,60,82,233,122,213,204,151,55,102,95,16,69,127,175,44,91,245,45,54,84,26,15,237,47,236,193,251,86,124,119,211,124,5,117,5,159,252,19,67,246,219,241,4,71,78,145,126,217,164,120,243,254,9,195,13,186,143,34,33,189,147,94,255,0,130,128,216,205,176,14,120,136,177,221,192,206,107,242,231,254,11,53,241,47,195,159,13,127,224,180,254,18,191,241,22,155,241,3,82,130,247,254,9,227,251,57,172,17,248,3,225,63,197,63,139,23,136,214,191,180,183,237,160,210,125,179,77,248,91,224,205,102,227,79,82,38,79,45,174,34,137,102,33,214,34,230,57,54,126,162,254,204,31,240,82,255,0,248,39,15,135,252,7,119,103,175,127,193,64,127,98,77,18,237,180,233,17,109,117,127,218,179,224,78,155,114,206,97,140,4,88,47,60,120,140,91,32,140,99,60,87,200,231,177,189,118,254,168,235,183,107,53,207,174,145,254,95,187,228,116,81,255,0,21,190,238,255,0,215,222,126,58,127,193,71,255,0,104,15,139,30,32,208,124,92,154,191,236,61,251,81,120,25,101,179,213,196,147,120,167,197,191,177,93,228,86,161,225,156,51,76,60,21,251,95,235,14,81,65,59,188,180,144,225,78,208,199,175,249,223,252,71,150,233,190,33,248,241,175,96,130,222,241,188,103,226,134,187,130,214,230,75,187,104,110,142,185,124,103,138,222,238,91,72,30,234,5,148,184,73,26,8,89,213,67,52,81,146,80,127,162,15,252,20,127,246,216,253,147,62,33,104,94,47,182,248,75,241,255,0,225,167,199,91,249,172,181,119,125,31,246,120,241,12,31,180,46,191,97,98,246,243,198,218,246,179,160,124,18,93,126,247,66,240,180,115,201,107,111,54,175,123,111,6,151,5,206,165,103,107,53,218,92,222,218,71,63,249,222,252,71,185,130,243,226,31,143,110,226,75,132,138,235,198,158,40,185,141,46,173,110,172,110,86,57,245,203,233,81,110,44,175,98,142,107,57,194,176,15,20,209,164,177,176,41,34,43,130,163,209,201,32,227,70,87,161,236,111,109,61,239,253,187,250,238,69,93,247,191,252,50,63,223,10,138,40,175,116,200,43,252,181,127,224,151,95,242,79,254,29,255,0,216,135,225,191,253,54,105,53,254,165,85,254,90,191,240,75,175,249,39,255,0,14,255,0,236,67,240,223,254,155,52,154,243,51,95,247,117,234,92,55,126,135,245,147,251,57,127,170,176,250,39,243,90,253,203,248,79,255,0,32,247,255,0,175,89,255,0,244,68,149,248,105,251,57,127,170,176,250,39,243,90,253,203,248,79,255,0,32,247,255,0,175,89,255,0,244,68,149,249,254,43,121,122,255,0,145,213,13,159,169,243,79,237,11,255,0,30,119,159,245,216,255,0,232,185,171,240,23,246,157,255,0,85,127,254,236,159,250,12,149,251,245,251,66,255,0,199,157,231,253,118,63,250,46,106,252,5,253,167,127,213,95,255,0,187,39,254,131,37,116,96,55,94,171,244,21,78,135,228,167,252,18,143,254,83,193,251,0,255,0,216,203,251,76,255,0,235,21,126,210,181,254,146,53,254,109,223,240,74,63,249,79,7,236,3,255,0,99,47,237,51,255,0,172,85,251,74,215,250,72,215,232,88,79,247,122,126,135,44,254,38,20,81,69,116,146,20,81,69,0,127,59,191,240,81,159,248,39,207,237,57,241,131,254,10,45,224,15,218,179,225,23,130,172,254,33,252,60,212,63,101,207,0,252,13,241,5,158,157,226,207,5,248,119,93,240,87,137,62,22,124,91,248,209,241,4,234,154,213,167,142,124,73,165,46,165,163,107,26,95,198,171,72,52,243,165,54,161,52,87,30,17,212,134,165,13,140,114,105,210,94,253,175,251,42,248,178,251,70,240,20,150,254,36,248,97,241,231,194,242,93,89,180,17,67,174,252,4,248,187,99,122,75,91,66,193,155,77,127,8,121,240,117,43,182,104,225,59,193,86,219,134,43,250,151,69,121,120,220,170,142,54,126,210,85,103,74,110,215,229,125,172,151,228,92,38,225,178,185,252,207,254,217,95,179,63,198,191,218,103,81,241,127,129,254,19,124,35,248,191,170,106,247,26,6,175,170,193,121,226,63,135,119,159,13,252,55,117,102,94,202,205,162,179,241,143,197,13,75,67,209,166,213,254,211,172,91,121,122,113,212,18,254,120,226,184,158,59,99,111,103,121,44,31,205,93,223,252,25,147,255,0,5,9,241,245,221,207,142,175,63,104,31,217,95,192,247,126,53,184,155,197,183,94,10,241,62,175,241,34,243,196,190,15,185,241,28,141,172,79,225,111,16,221,248,67,192,122,166,147,117,174,105,242,222,53,165,220,154,94,167,168,233,207,113,105,35,89,95,93,219,24,174,36,255,0,75,74,43,124,30,6,158,14,46,48,156,166,222,238,76,82,155,155,187,10,40,162,187,73,10,255,0,45,95,248,37,215,252,147,255,0,135,127,246,33,248,111,255,0,77,154,77,127,169,85,127,150,175,252,18,235,254,73,255,0,195,191,251,16,252,55,255,0,166,205,38,188,204,215,253,221,122,151,13,223,161,253,100,254,206,95,234,172,62,137,252,214,191,114,254,19,255,0,200,61,255,0,235,214,127,253,17,37,126,26,126,206,95,234,172,62,137,252,214,191,114,254,19,255,0,200,61,255,0,235,214,127,253,17,37,126,127,138,222,94,191,228,117,67,103,234,124,211,251,66,255,0,199,157,231,253,118,63,250,46,106,252,5,253,167,127,213,95,255,0,187,39,254,131,37,126,253,126,208,191,241,231,121,255,0,93,143,254,139,154,191,1,127,105,223,245,87,255,0,238,201,255,0,160,201,93,24,13,215,170,253,5,83,161,249,41,255,0,4,163,255,0,148,240,126,192,63,246,50,254,211,63,250,197,95,180,173,127,164,141,127,155,119,252,18,143,254,83,193,251,0,255,0,216,203,251,76,255,0,235,21,126,210,181,254,146,53,250,22,19,253,222,159,161,203,63,137,133,20,81,93,36,133,20,81,64,5,20,81,64,5,20,81,64,5,20,81,64,24,158,38,241,55,135,60,23,225,207,16,120,199,198,62,32,209,60,39,225,31,9,232,154,175,137,188,85,226,175,19,106,182,26,23,135,60,53,225,205,10,194,227,84,215,60,65,226,13,115,84,184,138,215,70,209,44,180,203,91,171,155,187,187,153,99,130,222,11,105,38,154,68,141,25,135,249,127,127,193,50,116,189,75,75,240,47,128,109,181,45,62,251,78,184,183,240,87,135,237,110,32,190,180,158,210,104,46,97,211,180,196,150,222,104,167,141,90,57,213,227,112,200,64,101,40,65,0,131,95,234,3,226,95,13,120,115,198,158,28,241,7,131,188,99,225,253,19,197,158,17,241,102,137,170,248,107,197,62,22,241,46,149,97,174,248,115,196,190,28,215,108,39,210,245,207,15,248,131,67,213,32,150,215,89,209,47,52,203,171,171,107,187,75,152,164,130,226,11,153,33,154,55,141,217,79,249,184,234,191,7,254,32,127,193,61,63,105,111,28,254,203,95,25,227,241,68,90,199,129,245,219,195,224,111,21,107,54,95,216,86,95,23,126,22,92,107,122,165,135,195,255,0,139,158,25,183,181,214,181,43,111,236,77,127,72,211,86,111,179,91,234,55,237,165,106,144,106,126,26,212,46,87,92,210,47,45,87,206,204,161,41,208,180,85,210,223,240,46,27,250,159,210,119,236,225,111,59,193,167,186,65,51,161,8,3,44,110,202,78,224,48,24,46,15,32,143,168,175,220,79,133,4,13,61,201,32,15,179,76,50,72,3,38,39,80,57,238,88,128,61,206,43,249,166,253,155,191,104,109,26,203,79,176,198,161,152,74,7,70,146,234,89,102,86,72,247,200,17,67,224,198,202,167,144,167,203,146,76,178,178,191,31,171,126,7,253,169,52,171,59,60,197,123,10,199,26,53,180,143,3,9,29,230,85,137,201,197,190,0,140,199,38,62,103,221,145,202,0,213,240,88,170,51,230,122,110,255,0,200,233,131,73,91,169,237,159,180,20,111,37,165,226,198,142,236,37,36,132,82,196,13,146,140,225,71,76,145,249,215,224,47,237,65,28,145,199,126,178,70,241,182,201,14,29,89,78,49,32,206,24,116,200,35,240,175,211,15,138,191,180,190,147,113,13,194,189,220,95,105,84,104,228,72,36,45,36,204,240,27,149,69,69,84,148,146,139,180,110,8,161,246,177,102,92,3,248,103,251,87,124,126,209,36,139,81,158,227,81,147,123,71,39,250,155,249,76,49,70,112,136,99,57,102,101,203,128,48,9,149,229,249,19,115,170,86,248,26,85,19,94,235,221,126,130,155,78,214,103,198,223,240,74,139,43,200,63,224,187,159,176,12,179,218,92,195,17,241,71,237,49,24,146,88,37,142,50,231,246,41,253,165,216,32,119,64,11,149,71,32,103,56,83,232,107,253,34,43,248,235,255,0,131,116,127,100,207,20,248,255,0,227,223,196,255,0,248,40,39,141,124,45,117,15,195,31,14,120,47,93,248,69,251,61,106,254,48,240,190,155,168,199,226,239,30,120,147,196,105,109,241,63,226,55,195,61,119,81,213,13,222,131,39,134,124,61,225,91,191,9,93,106,150,54,18,218,106,207,241,95,196,122,21,182,178,178,232,62,32,211,166,254,197,43,239,240,177,148,104,83,82,86,118,57,165,241,48,162,138,43,160,144,162,138,40,0,162,138,40,0,162,138,40,0,162,138,40,0,175,136,127,110,143,248,39,207,236,233,255,0,5,9,248,119,160,120,11,227,190,147,174,216,234,94,11,215,87,196,63,15,126,40,120,6,247,72,208,254,41,124,61,187,186,159,79,30,37,177,240,199,136,53,157,11,82,181,127,14,235,154,94,157,111,101,173,105,90,133,133,254,155,122,150,214,119,198,214,61,99,72,209,53,61,52,162,147,73,166,154,186,96,127,52,191,22,255,0,224,135,31,240,80,111,217,188,248,122,247,246,116,241,231,132,63,108,125,14,88,244,203,45,102,194,49,162,124,2,248,139,165,234,210,67,226,41,111,47,78,129,241,19,226,5,222,129,168,120,50,220,88,232,140,215,131,197,82,106,243,95,120,136,91,71,225,225,103,103,54,169,95,159,159,30,190,48,126,212,159,176,247,140,52,239,133,31,181,103,195,189,71,225,159,196,63,16,248,106,207,226,30,141,160,201,226,127,2,120,252,222,120,47,86,212,245,159,13,233,218,183,246,207,195,223,20,106,86,86,203,38,185,225,47,17,67,246,89,103,91,148,254,207,243,94,21,134,104,30,66,138,243,177,24,12,52,147,151,37,157,215,161,164,36,219,179,212,245,239,134,159,1,255,0,224,167,63,181,247,194,237,31,227,135,192,15,217,215,80,241,223,194,15,136,113,107,43,224,223,22,183,197,207,129,94,19,142,254,127,11,235,90,167,130,245,169,135,134,124,97,241,55,78,213,52,243,109,226,175,15,107,54,248,158,8,12,255,0,98,243,161,102,183,154,57,91,244,163,246,119,255,0,131,114,60,81,226,175,19,63,138,63,224,160,95,29,180,143,18,248,115,77,215,60,253,63,224,167,236,223,168,248,146,215,67,241,101,133,165,239,132,117,91,121,124,109,241,119,197,254,28,210,181,109,63,70,190,137,60,125,163,234,90,54,135,162,233,250,170,90,106,150,90,150,151,227,123,11,175,58,206,50,138,215,15,130,195,211,80,156,97,239,52,159,206,200,151,38,250,159,212,215,134,188,53,225,207,5,248,115,195,254,14,240,119,135,244,79,9,248,71,194,122,38,149,225,175,11,120,91,195,90,85,134,133,225,207,13,120,115,66,176,131,75,208,252,63,225,253,15,75,130,43,93,27,68,179,211,45,109,109,173,45,45,162,142,11,120,45,163,134,24,210,52,85,27,116,81,93,164,133,20,81,64,5,20,81,64,5,20,81,64,5,20,81,64,31,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 8952; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

