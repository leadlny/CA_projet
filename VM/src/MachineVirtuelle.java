import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.IntBuffer;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class MachineVirtuelle {
	private int[] registres;
	private int[] tableau0;
	private HashMap<Integer, int[]> collec;// map qui réprésente la collec avec (index,tableau)
	private int pc = 0;
	private String filename;
	private LinkedList<Integer> StockIndex;

	// enum OPERATORS {MOVE, IND_TAB, MODIF_TAB, ADD, MULT, DIV, NAND, STOP, ALLOC,
	// ABORT, OUT, IN, LOAD, ORTHO};

	public MachineVirtuelle(String f) {
		registres = new int[8];
		collec = new HashMap<Integer, int[]>();
		filename = f;
		StockIndex = new LinkedList<Integer>();
		Arrays.fill(registres, 0); // rempli les registres à 0
		readProgram(f);
		collec.put(0, tableau0);

		call();

	}

	public void readProgram(String filePath) {
		// file to byte[], Path
		File f;
		FileInputStream br = null;
		Byte defaultByte = (byte) 0;
		try {
			f = new File(filePath);
			byte[] bytes = new byte[(int) f.length()];
			br = new FileInputStream(f);
			br.read(bytes);

			ByteBuffer bb = ByteBuffer.wrap(bytes);
			for (int i = 0; i < f.length() % 4; ++i) {
				bb = bb.put(defaultByte);
			}
			IntBuffer ib = bb.asIntBuffer();
			tableau0 = new int[ib.limit()];
			ib.get(tableau0);

		} catch (IOException e) {
			e.printStackTrace();
		} finally {
			try {
				if (br != null) {
					br.close();
				}
			} catch (IOException ex) {
				ex.printStackTrace();
			}
		}
	}

	private void move(int a, int b, int c) {
		 //System.out.println("MOVE");
		if (registres[c] != 0) {
			registres[a] = registres[b];
		}
	}

	private void index(int a, int b, int c) {
		// System.out.println("INDEX");
		if (registres[b] == 0) {
			registres[a] = tableau0[registres[c]];

		} else {
			registres[a] = collec.get(registres[b])[registres[c]];
		}
	}

	private void modif(int a, int b, int c) {
		// System.out.println("MODIF");
		if (registres[a] == 0) {
			tableau0[registres[b]] = registres[c];

		} else {
			collec.get(registres[a])[registres[b]] = registres[c];
		}
	}

	private void add(int a, int b, int c) {
		 //System.out.println("ADD");
		registres[a] = registres[b] + registres[c];
	}

	private void mult(int a, int b, int c) {
		// System.out.println("MULT");
		registres[a] = registres[b] * registres[c];
	}

	private void div(int a, int b, int c) {
		// System.out.println("DIV");
		registres[a] = Integer.divideUnsigned(registres[b], registres[c]);
	}

	private void nand(int a, int b, int c) {
		// System.out.println("NAND");
		registres[a] = ~(registres[b] & registres[c]);
	}

	private void alloc(int b, int c) {
		// System.out.println(" b:"+b+" c:"+c);
		// System.out.println("ALLOC");
        int rc = registres[c];
        int[] tab = new int[rc];
        Integer id=StockIndex.pollFirst();
        if(id==null) {id=collec.size();}
        //insert into map
        collec.put(id, tab);
        if (id == 0) {
            tableau0 = tab;
        }
        //store the new id in register b
        registres[b] = id;
	}

	private void aban(int c) {
		// System.out.println("ABANDON");
		collec.remove(registres[c]);
		StockIndex.add(registres[c]);

	}

	private void exit(int c) {
		// System.out.println("SORTIE");
		if (registres[c] > 0 && registres[c] < 255) {
			System.out.print((char) registres[c]);
		}
	}

	private void entry(int c) {
		// System.out.println("ENTREE");

	}

	private void load(int b, int c) {
		// System.out.println("LOAD");
		if (registres[b] != 0) {
			int[] tmp = collec.get(registres[b]);
			tableau0 = tmp.clone();
			collec.put(0, tableau0);
		}
		pc = registres[c];

	}

	private void ortho(int a, int value) {
		 //System.out.println("ORTHO");
		registres[a] = value;
	}

	private void call() {
    	
    		
    	while(pc < tableau0.length) {
    		
    		int current = tableau0[pc];
    		
    		int op = (current >> 28) & 0b1111;
    		
    		if (op == 13){
    			int a = (current >> 25) & 0b111;
    			int value = (current & 0b1111111111111111111111111);
    			//System.out.println("op: "+op+" a: "+a+" value:"+value);
    			ortho(a, value);
    			pc++;
    			
    		} else {
    			int a = (current >> 6) & 0b111;
    			int b = (current >> 3) & 0b111;
    			int c = current & 0b111;
    			//System.out.println("op: "+op+" a: "+a+" b:"+b+" c:"+c);
    			switch (op){
    				case 0:
    					move(a, b, c);pc++;
    					break;
    				case 1:
    					index(a, b, c);pc++;
       					break;
    				case 2:
    					modif(a, b, c);pc++;
    					break;
    				case 3:
    					add(a, b, c);pc++;
    					break;
    				case 4:
    					mult(a, b, c);pc++;
    					break;
    				case 5:
    					div(a, b ,c);pc++;
    					break;
    				case 6:
    					nand(a, b, c);pc++;
    					break;
    				case 7:
    					System.exit(1); //jsp quoi mettre en java
    				case 8:
    					alloc(b, c);pc++;
    					break;
    				case 9:
    					aban(c);;pc++;
    					break;
    				case 10:
    					exit(c);pc++;
    					break;
    				case 11:
    					entry(c);pc++;
    					break;
    				case 12:
    					load(b,c);
    					break;
    				default: System.out.println("Erreur, instruction inconnue");
    					
    			}
    			
    		}
    		
    		
    	}
    }


	public static void main(String[] args) {
		String filename = args[0]; // pour pouvoir le faire avec n'importe quel fichier
		MachineVirtuelle vm = new MachineVirtuelle(filename);
		System.out.println();

	}

}
