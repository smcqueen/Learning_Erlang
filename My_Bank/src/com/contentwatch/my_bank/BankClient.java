package com.contentwatch.my_bank;

import java.io.IOException;

import com.ericsson.otp.erlang.*;

public class BankClient {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		String serverName = "my_bank";
		
		try {
			OtpSelf client = new OtpSelf("client");
			OtpPeer server = new OtpPeer("server");
			OtpConnection conn = client.connect(server);
			conn.sendRPC(serverName, "start", new OtpErlangList());
			System.out.println("Starting my_bank, response = " + conn.receiveMsg().getMsg().toString());
			conn.sendRPC(serverName, "new_account", new OtpErlangList(new OtpErlangAtom("stan")));
			System.out.println("New account: " + conn.receiveMsg().getMsg().toString());
			conn.sendRPC(serverName, "deposit", 
					new OtpErlangList(new OtpErlangObject[]{
							new OtpErlangAtom("stan"), 
							new OtpErlangInt(30)
						}));
			OtpErlangObject response = conn.receiveRPC();
			if (response instanceof OtpErlangTuple) {
				OtpErlangTuple tuple = (OtpErlangTuple) response;
				for (int i = 0; i < tuple.arity(); i++) {
					System.out.println(String.format("deposit: Element %d = %s", i, tuple.elementAt(i).toString()));
				}
			}
			
			conn.sendRPC(serverName, "deposit", 
					new OtpErlangList(new OtpErlangObject[]{
						new OtpErlangAtom("stan"), 
						new OtpErlangInt(20)
					}));
			response = conn.receiveRPC();
			if (response instanceof OtpErlangTuple) {
				OtpErlangTuple tuple = (OtpErlangTuple) response;
				for (int i = 0; i < tuple.arity(); i++) {
					System.out.println(String.format("deposit: Element %d = %s", i, tuple.elementAt(i).toString()));
				}
			}
			
			conn.sendRPC(serverName, "withdraw", 
					new OtpErlangList(new OtpErlangObject[]{
							new OtpErlangAtom("stan"), 
							new OtpErlangInt(30)
						}));
			response = conn.receiveRPC();
			if (response instanceof OtpErlangTuple) {
				OtpErlangTuple tuple = (OtpErlangTuple) response;
				for (int i = 0; i < tuple.arity(); i++) {
					System.out.println(String.format("withdraw: Element %d = %s", i, tuple.elementAt(i).toString()));
				}
			}
			
			conn.sendRPC(serverName, "withdraw", 
					new OtpErlangList(new OtpErlangObject[]{
							new OtpErlangAtom("stan"), 
							new OtpErlangInt(30)
						}));
			response = conn.receiveRPC();
			if (response instanceof OtpErlangTuple) {
				OtpErlangTuple tuple = (OtpErlangTuple) response;
				for (int i = 0; i < tuple.arity(); i++) {
					System.out.println(String.format("withdraw: Element %d = %s", i, tuple.elementAt(i).toString()));
				}
			}
			
			conn.sendRPC(serverName, "stop", new OtpErlangList());
			System.out.println("System shutdown");
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (OtpAuthException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (OtpErlangDecodeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (OtpErlangExit e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

}
