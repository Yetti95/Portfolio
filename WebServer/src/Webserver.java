/**
 * A simple program demonstrating server sockets.
 *
 * @author Greg Gagne.
 */

import java.net.*;
import java.util.concurrent.*;

public class Webserver
{
	 
	public static final int DEFAULT_PORT = 8080;
    private static final Executor exec = Executors.newCachedThreadPool();
	public static void main(String[] args) throws java.io.IOException {
		// create a server socket listening to port 2500
		boolean bool = true;
		ServerSocket server = new ServerSocket(DEFAULT_PORT);
		System.out.println("Waiting for connections ....");
        Configuration config;
        
        try{
            config = new Configuration(args[0]);
        }
        catch(ConfigurationException ce){
            System.out.println("Error in " + args[0] + ": set up");
            return;
        }
        Socket client = server.accept();
		while (bool) {
			// we block here until there is a client connection
			Connection connection = new Connection(client, config);
            exec.execute(connection);
            //execute the connection using the exec method from proxy server



			/**
			 * we have a connection!
			 * Let's get some information about it. 
			 * An InetAddress is an IP address
			 */ 	
			
			// get the server-side info
			// System.out.print(InetAddress.getLocalHost() + " : ");
			// System.out.println(server.getLocalPort());
			
			// get the client-side info
			// InetAddress ipAddr = client.getInetAddress();
			// System.out.print(ipAddr.getHostAddress() + " : ");
			// System.out.println(client.getPort());
			
			// close the socket
			//client.close();
			server.close();
			bool = false;
		}
	}
}
