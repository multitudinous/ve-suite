import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import org.omg.CosNaming.*;
import java.util.Properties;
import org.omg.CORBA.*;
import java.io.*;
import java.lang.String;
import java.lang.Thread;
import javax.rmi.CORBA.*;

public class my_orb
{
   static String hostname,portnum;
   static ObserverServant obsref;
   static anorb obj_vis;
   static anorb[] obj_dst;
   static ObserverServant[] recobj;
   static short dest_num;
   static JFrame fr;
   static String[] agrss;
   //boolean inWaiting;
   //static org.omg.CORBA.ORB orb;
   //static NamingContext initContext;

   my_orb()
   {
      //agrss = temp;
      try
      {
         read_config();
      }
      catch(IOException ioe)
      {
         System.out.println("Check your config:"+ioe);
      }
   } 

   public void pass_main_frame(JFrame f)
   {
      this.fr = f;
   }

   public void set_vis()
   {
      System.out.println(hostname);
      obsref = new ObserverServant(this.fr);
      if(!hostname.equalsIgnoreCase("0"))
      {
         System.out.println("uses Master VE_Xplorer");
         obj_vis = new anorb(obsref,"Master","VE_Xplorer"); //changed
      }
      else
      {
         System.out.println("uses other");
         obj_vis = new anorb(obsref);
      }
      obj_vis.start();
   }

   void set_dest(short n, String[] dst_name)
   {
      this.my_wait();
      dest_num = n;
      if(dest_num >0)
      {
         obj_dst = new anorb[dest_num];
         recobj = new ObserverServant[dest_num];

         //System.out.println("dest_num:"+dest_num);
         //System.out.println("dest_name:"+dst_name[0]);

         for (int i = 0; i < dest_num; i++)
         {
            recobj[i] = new ObserverServant(this.fr);
            obj_dst[i] = new anorb(recobj[i],"Recv",dst_name[i]);
            obj_dst[i].start();
         }
      }
   }

   synchronized protected void my_wait()
   {
      Thread thd = Thread.currentThread();
      System.out.println(thd.toString());

      try
      {
         wait();
      }
      catch (InterruptedException e) { }
   }

   synchronized protected void my_notify()
   {
      Thread thd = Thread.currentThread();
      System.out.println(thd.toString());

      notifyAll();
   }

   void disconnect()
   {
      //obj_vis.disconnect();
  
      if(dest_num>0)
         for (int i = 0; i < dest_num; i++)
         {
            obj_dst[dest_num-1-i].disconnect();
         }

      obj_vis.disconnect();
   }

   static void read_config() throws IOException
   {
 		String remote_host,port_num;
				  
		FileInputStream configFile = new FileInputStream("./config/comm.config");
		BufferedReader r = new BufferedReader(new InputStreamReader(configFile));
		
		remote_host = r.readLine();
		if(remote_host.startsWith("host_name"))
		 hostname = remote_host.substring(10);
		
		port_num = r.readLine();
		if(port_num.startsWith("port_number"))
		 portnum = port_num.substring(12);
				
		r.close();
   }

   public class anorb extends Thread
   {
      //static int exit_status=0;
	   //static int counter=0;
	   //Hello hello;
      short clientInfoArray[];
      VjObs testObs;
	   //static FileInterfaceOperations fileRef;
	   //static org.omg.CORBA.Object initRef=null;
	   //static NamingContext initContext=null;

	   org.omg.CORBA.ORB orb;
	
	   ObserverServant myos;
	   String my_dst1;
	   String my_dst2;
	
	   boolean no_ns;

	   anorb(ObserverServant o,String dst_name1, String dst_name2)
	   {
	      myos = o;
	      my_dst1 = dst_name1;
	      my_dst2 = dst_name2;
	      no_ns = false;
         // Defined to be the same size as the array in VjObs_i.h
         clientInfoArray = new short[ 9 ];
	   }

	   anorb(ObserverServant o)
	   {
	      myos = o;
	      no_ns = true;
	   }

	   public void disconnect()
      {
	      //this.testObs.detach(myos);
      }
	
	   public void send_id()
      {
         System.out.println("send in orb" );
		   //this.hello.update();
         //testObs.SetClientInfoFlag( (short)1 );
         clientInfoArray[ 0 ] = (short)myos.id;
         System.out.println(" command id : " + clientInfoArray[ 0 ] );
         clientInfoArray[ 1 ] = (short)myos.iso_value;
         System.out.println(" iso_value  : " + clientInfoArray[ 1 ] );
         clientInfoArray[ 2 ] = (short)myos.timesteps;
         System.out.println(" timesteps  : " + clientInfoArray[ 2 ] );
         clientInfoArray[ 3 ] = (short)myos.sc;
         System.out.println(" sc         : " + clientInfoArray[ 3 ] );
         clientInfoArray[ 4 ] = (short)myos.min;
         System.out.println(" min        : " + clientInfoArray[ 4 ] );
         clientInfoArray[ 5 ] = (short)myos.max;
         System.out.println(" max        : " + clientInfoArray[ 5 ] );
         clientInfoArray[ 6 ] = (short)myos.geo_state;
         System.out.println(" geo_state  : " + clientInfoArray[ 6 ] );
         clientInfoArray[ 7 ] = (short)myos.pre_state;
         System.out.println(" pre_state  : " + clientInfoArray[ 7 ] );
         clientInfoArray[ 8 ] = (short)myos.teacher_state;
         System.out.println(" teacher_state : " + clientInfoArray[ 8 ] );
         testObs.SetClientInfoData( clientInfoArray );
      }
			
      synchronized public void run()
	   {
         String[] temp = new String[2];
         String pieces1 = new String("NameService=corbaname::");
         String pieces2 = new String(":");
         temp[0] = new String("-ORBInitRef");
         temp[1] = new String();
		   temp[1] = pieces1 + hostname + pieces2 + portnum;
         int status=0;
		   orb=null;

		   try
		   {
		      if ( !no_ns )
		      {
			      // Doesn't seem to work with omniORB 4
               // I may not know how do do this properly with 
               // omniORB 4
               //Properties props=new Properties();
   			   //props.put("ORBInitialPort",portnum);
   			   //props.put("ORBInitialHost",hostname);
   			   //props.list( System.out );
               System.out.println("ORB Flag : " + temp[0] );
               System.out.println("Port and computer : " + temp[1] );
               orb = org.omg.CORBA.ORB.init( temp, null );
			      status=start_orb(my_dst1,my_dst2);
		      }
		      else
		      {
			      FileInputStream iorFile = new FileInputStream("./config/host.ior");
			      BufferedReader r = new BufferedReader(new InputStreamReader(iorFile));
			      String ior_str;
			      ior_str = r.readLine();
			      r.close();

			      orb = org.omg.CORBA.ORB.init(temp,null);
			      org.omg.CORBA.Object tmpobj= orb.string_to_object(ior_str);
			      testObs = VjObsHelper.narrow(tmpobj);
		      }
			
			   orb.connect(myos);
            System.out.println(" object to string : " + orb.object_to_string(myos) );
			   //myos.connect(orb);
            myos.id = -1;
			   //testObs.attach(myos);
			
			   //java.lang.Object sync=new java.lang.Object();
			   //synchronized (this){
				my_notify();
				wait();			
		   }
		   catch(Exception ex)
		   {
			   System.err.println("Bad luck!" + ex);
			   ex.printStackTrace();
			   status=1;
		   }

		   //System.exit(status);
	   }

/*public static void init_fileComm(String filename)throws IOException
	{
	String[] temp = null; 
	read_config();
	org.omg.CORBA.Object fileobjRef = null;
	org.omg.CORBA.Object nc_context = null;
	NamingContext ncRef = null;
	try
	 {
	  Properties props=new Properties();
	  props.put("org.omg.CORBA.ORBInitialPort",portnum);
  	  props.put("org.omg.CORBA.ORBInitialHost",hostname);
	  org.omg.CORBA.ORB my_orb = org.omg.CORBA.ORB.init(temp,props);

	  fileobjRef =my_orb.resolve_initial_references("NameService");
          ncRef = NamingContextHelper.narrow(fileobjRef);

          NameComponent[] path=new NameComponent[2];
	  path[0]=new NameComponent("test","my_context");
	  path[1]=new NameComponent("FileComm","FileObject");      
          nc_context = ncRef.resolve(path);
          FileInterfaceOperations fileRef =FileInterfaceHelper.narrow(nc_context);

	//Save the file
	  //File file = new File(filename);
	  int file_size = fileRef.get_file_size(filename);
	  BufferedOutputStream output = new BufferedOutputStream(new FileOutputStream(filename));
	  byte[] buffer;
	  buffer = new byte[file_size];
	System.out.println(file_size);  
	  for ( int i = 0; i < file_size; i++)
	  {
           buffer[i] = (byte)fileRef.downloadFile();
	  }
	  
	  output.write(buffer, 0, file_size);
	  output.flush();
	  output.close();

	  fileRef.close();
	 }
	 catch(Exception e) {
                          System.out.println("FileClient Error: " + e.getMessage());
                          e.printStackTrace();
         }
	}*/

	   int start_orb(String s1,String s2)
	   {
	      org.omg.CORBA.Object initRef=null;
  	      try
  	      {
            initRef=orb.resolve_initial_references("NameService");
      
            String tmp_str = orb.object_to_string(initRef);
	         System.out.println("|   Initial connection to orb : " + tmp_str);
  	      }
  	      catch(org.omg.CORBA.SystemException se)
  	      {
   	      System.err.println("Resolve init failure " + se);
   	      System.exit(1);
  	      }
  	      catch(org.omg.CORBA.UserException ue)
  	      {
            System.err.println("Resolve init failure " + ue);
            ue.printStackTrace( System.out );
   	      System.exit(1);
  	      }

  	      NamingContext initContext=null;

  	      try
  	      {
   	      initContext=NamingContextHelper.narrow(initRef);
  	      }
  	      catch(org.omg.CORBA.SystemException se)
  	      {
   	      System.err.println("Context narrow failure " + se);
   	      System.exit(1);
  	      }

	      NameComponent[] name=new NameComponent[1];
	      //name[0]=new NameComponent("test","my_context");
	      //name[1]=new NameComponent("FileComm","FileObject");
	      name[0]=new NameComponent(s1,s2);
	      //NameComponent nc = new NameComponent("Master","VE_Xplorer");  
	      //NameComponent path[] = {nc};

         org.omg.CORBA.Object objRef=null;
	      try
         {
		      objRef=initContext.resolve(name);
		      //objRef=initContext.resolve(path);
            System.out.println("|   Initial connection to orb 2 : " + orb.object_to_string(objRef) );
	      }
	      catch(org.omg.CORBA.SystemException se)
         {
		      System.err.println("Resolve name failure System Exception : " + se);
		      System.exit(1);
	      }
         catch(org.omg.CORBA.UserException ue)
         {
		      System.err.println("Resolve name failure User Exception : " + ue);
		      org.omg.CosNaming.NamingContextPackage.NotFound ve = (org.omg.CosNaming.NamingContextPackage.NotFound)ue;
		      NameComponent[] tmp_name = ve.rest_of_name;
		      for(int i = 0; i < 1; ++i)
		      {
		         System.err.println(tmp_name[i].id);
		         System.err.println(tmp_name[i].kind);
		      }
		      System.exit(1);
	      }
	 
         try
         {
		      testObs=VjObsHelper.narrow(objRef);
            System.out.println("|   Initial connection to orb 3 : " + orb.object_to_string(testObs) );
	      }
         catch(org.omg.CORBA.SystemException se)
         {
		      System.err.println("Hello narrow failure " + se);
		      System.exit(1);
	      }
		
	      return 0;
	   }
   }
}
	
