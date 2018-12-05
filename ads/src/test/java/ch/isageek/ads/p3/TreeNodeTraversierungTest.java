package ch.isageek.ads.p3;

import static org.junit.Assert.*;

import java.util.ArrayList;

import org.junit.Test;



public class TreeNodeTraversierungTest {

	BinaryTree<Integer> emptyTree = new BinaryTree<Integer>();
	BinaryTree<Integer> onlyRootNodeTree = new BinaryTree<Integer>(42);
	BinaryTree<Integer> twoNodeTree;
	BinaryTree<Integer> manyNodeTree;
	
	ArrayList<Integer> manyNodeTreeListTraversedByInorder,
							manyNodeTreeListTraversedByPreorder,
							manyNodeTreeListTraversedByPostorder,
							manyNodeTreeListTraversedByLevelorder;
	
	
	@Test
	public void shouldReturnAEmptyListWhenTraversingAnEmptyTree(){
		assertTrue(emptyTree.traversePostorder().isEmpty());
		assertTrue(emptyTree.traverseLevelorder().isEmpty());
		assertTrue(emptyTree.traversePreorder().isEmpty());
		assertTrue(emptyTree.traversePostorder().isEmpty());
	}
	
	@Test
	public void shouldTraverseOnlyRootNodeTreeCorrectly() {
		
		ArrayList<Integer> rootNodeTree = new ArrayList<Integer>();
		rootNodeTree.add(42);
				
		assertEquals(rootNodeTree, onlyRootNodeTree.traversePostorder());
		assertEquals(rootNodeTree, onlyRootNodeTree.traversePreorder());
		assertEquals(rootNodeTree, onlyRootNodeTree.traverseInorder());
		assertEquals(rootNodeTree, onlyRootNodeTree.traverseLevelorder());
	}
	
	
	@Test
	public void shouldInLevelorderTraverseTwoNodeTreeCorretly() {
		
		ArrayList<Integer> testArrayList = new ArrayList<Integer>();
		testArrayList.add(12);
		testArrayList.add(56);
		testArrayList.add(77);
		
		createTwoNodeBinaeryTree();
		ArrayList<Integer> levelorderArrayList = twoNodeTree.traverseLevelorder();
		
		assertEquals(testArrayList, levelorderArrayList);
	}

	
	@Test
	public void shouldInPostorderTraverseTwoNodeTreeCorretly(){
		ArrayList<Integer> testArrayList = new ArrayList<Integer>();
		testArrayList.add(56);
		testArrayList.add(77);
		testArrayList.add(12);
		
		createTwoNodeBinaeryTree();
		ArrayList<Integer> postorderArrayList = twoNodeTree.traversePostorder();
		
		assertEquals(testArrayList, postorderArrayList);
	}
	
	@Test
	public void shouldInPreorderTraverseTwoNodeTreeCorretly() {
		ArrayList<Integer> testArrayList = new ArrayList<Integer>();
		testArrayList.add(12);
		testArrayList.add(56);
		testArrayList.add(77);
		
		createTwoNodeBinaeryTree();
		ArrayList<Integer> preorderArrayList = twoNodeTree.traversePreorder();
		
		assertEquals(testArrayList,preorderArrayList);
	}
	
	@Test
	public void shouldInorderTraverseTwoNodeTreeCorretly(){
		
		ArrayList<Integer> testArrayList = new ArrayList<Integer>();
		testArrayList.add(56);
		testArrayList.add(12);
		testArrayList.add(77);
		
		createTwoNodeBinaeryTree();
		ArrayList<Integer> inorderArrayList = twoNodeTree.traverseInorder();
		
		assertEquals(testArrayList, inorderArrayList);
	}
	
	@Test
	public void shouldInorderTraverseManyNodeBinaryTreeCorrectly(){
		createManyNodeTree();
		createInOrderListforManyBinaryTree();
		
		ArrayList<Integer> list = manyNodeTree.traverseInorder();

		assertEquals(manyNodeTreeListTraversedByInorder, list);
	}
	
	@Test
	public void shouldPreorderTraverseManyNodeBinaryTreeCorrectly(){
		createManyNodeTree();
		createPreOrderListforManyBinaryTree();
		
		ArrayList<Integer> list = manyNodeTree.traversePreorder();
		
		assertEquals(manyNodeTreeListTraversedByPreorder, list);
		
	}
	
	@Test
	public void shouldPostorderTraverseManyNodeBinaryTreeCorrectly(){
		createManyNodeTree();
		createPostOrderListforManyBinaryTree();
		
		ArrayList<Integer> list = manyNodeTree.traversePostorder();
		
		assertEquals(manyNodeTreeListTraversedByPostorder, list);
		
	}
	
	@Test
	public void shouldLevelorderTraverseManyNodeBinaryTreeCorrectly(){
		createManyNodeTree();
		createLevelOrderListforManyBinaryTree();
		
		ArrayList<Integer> list = manyNodeTree.traverseLevelorder();
		
		assertEquals(manyNodeTreeListTraversedByLevelorder, list);
		
	}
	
	public void createTwoNodeTree(){
		twoNodeTree = new BinaryTree<Integer>(2);
		twoNodeTree.getRoot().setRight(new TreeNode<Integer>(6));
		twoNodeTree.getRoot().setLeft(new TreeNode<Integer>(42));
	}
	
	public void createManyNodeTree(){
		
		//Tree with 86 on top
		TreeNode<Integer> leafLeft99 = new TreeNode<Integer>(99);
		TreeNode<Integer> leafRight85 = new TreeNode<Integer>(85);
		TreeNode<Integer> node86 = new TreeNode<Integer>(86);
		node86.setLeft(leafLeft99);
		node86.setRight(leafRight85);
		
		//Tree with 73 on top of 86
		TreeNode<Integer> node73 = new TreeNode<Integer>(73);
		node73.setLeft(node86);
		
		//Tree with 37 on top of 73
		TreeNode<Integer> node37 = new TreeNode<Integer>(37);
		node37.setLeft(node73);
		
		//Tree with 32 on top of 13 and 37
		TreeNode<Integer> leafLeft13 = new TreeNode<Integer>(13);
		TreeNode<Integer> node32 = new TreeNode<Integer>(32);
		node32.setLeft(leafLeft13);
		node32.setRight(node37);
		
		
		//Tree with 83 on top tow leafs
		TreeNode<Integer> leafLeft55 = new TreeNode<Integer>(55);
		TreeNode<Integer> leafRight61 = new TreeNode<Integer>(61);
		TreeNode<Integer> node83 = new TreeNode<Integer>(83);
		node83.setLeft(leafLeft55);
		node83.setRight(leafRight61);
		
		//Tree with 22 on top of leaf 56 and tree 83
		TreeNode<Integer> leafLeft56 = new TreeNode<Integer>(56);
		TreeNode<Integer> node22 = new TreeNode<Integer>(22);
		node22.setLeft(leafLeft56);
		node22.setRight(node83);
		
		//Set Root node and attach to BinaryTree
		TreeNode<Integer> rootNode = new TreeNode<Integer>(12);
		rootNode.setLeft(node22);
		rootNode.setRight(node32);
		
		manyNodeTree = new BinaryTree<Integer>();
		manyNodeTree.setRoot(rootNode);
		
	}
	
	public void createLevelOrderListforManyBinaryTree(){
		manyNodeTreeListTraversedByLevelorder = new ArrayList<Integer>();
		manyNodeTreeListTraversedByLevelorder.add(12);
		manyNodeTreeListTraversedByLevelorder.add(22);
		manyNodeTreeListTraversedByLevelorder.add(32);
		manyNodeTreeListTraversedByLevelorder.add(56);
		manyNodeTreeListTraversedByLevelorder.add(83);
		manyNodeTreeListTraversedByLevelorder.add(13);
		manyNodeTreeListTraversedByLevelorder.add(37);
		manyNodeTreeListTraversedByLevelorder.add(55);
		manyNodeTreeListTraversedByLevelorder.add(61);
		manyNodeTreeListTraversedByLevelorder.add(73);
		manyNodeTreeListTraversedByLevelorder.add(86);
		manyNodeTreeListTraversedByLevelorder.add(99);
		manyNodeTreeListTraversedByLevelorder.add(85);
		
	}
	public void createPreOrderListforManyBinaryTree(){
		manyNodeTreeListTraversedByPreorder = new ArrayList<Integer>();
		manyNodeTreeListTraversedByPreorder.add(12);
		manyNodeTreeListTraversedByPreorder.add(22);
		manyNodeTreeListTraversedByPreorder.add(56);
		manyNodeTreeListTraversedByPreorder.add(83);
		manyNodeTreeListTraversedByPreorder.add(55);
		manyNodeTreeListTraversedByPreorder.add(61);
		manyNodeTreeListTraversedByPreorder.add(32);
		manyNodeTreeListTraversedByPreorder.add(13);
		manyNodeTreeListTraversedByPreorder.add(37);
		manyNodeTreeListTraversedByPreorder.add(73);
		manyNodeTreeListTraversedByPreorder.add(86);
		manyNodeTreeListTraversedByPreorder.add(99);
		manyNodeTreeListTraversedByPreorder.add(85);
	}
	public void createPostOrderListforManyBinaryTree(){
		manyNodeTreeListTraversedByPostorder = new ArrayList<Integer>();
		manyNodeTreeListTraversedByPostorder.add(56);
		manyNodeTreeListTraversedByPostorder.add(55);
		manyNodeTreeListTraversedByPostorder.add(61);
		manyNodeTreeListTraversedByPostorder.add(83);
		manyNodeTreeListTraversedByPostorder.add(22);
		manyNodeTreeListTraversedByPostorder.add(13);
		manyNodeTreeListTraversedByPostorder.add(99);
		manyNodeTreeListTraversedByPostorder.add(85);
		manyNodeTreeListTraversedByPostorder.add(86);
		manyNodeTreeListTraversedByPostorder.add(73);
		manyNodeTreeListTraversedByPostorder.add(37);
		manyNodeTreeListTraversedByPostorder.add(32);
		manyNodeTreeListTraversedByPostorder.add(12);
	}
	public void createInOrderListforManyBinaryTree(){
		manyNodeTreeListTraversedByInorder = new ArrayList<Integer>();
		manyNodeTreeListTraversedByInorder.add(56);
		manyNodeTreeListTraversedByInorder.add(22);
		manyNodeTreeListTraversedByInorder.add(55);
		manyNodeTreeListTraversedByInorder.add(83);
		manyNodeTreeListTraversedByInorder.add(61);
		manyNodeTreeListTraversedByInorder.add(12);
		manyNodeTreeListTraversedByInorder.add(13);
		manyNodeTreeListTraversedByInorder.add(32);
		manyNodeTreeListTraversedByInorder.add(99);
		manyNodeTreeListTraversedByInorder.add(86);
		manyNodeTreeListTraversedByInorder.add(85);
		manyNodeTreeListTraversedByInorder.add(73);
		manyNodeTreeListTraversedByInorder.add(37);
	}
	
	private void createTwoNodeBinaeryTree() {
		twoNodeTree = new BinaryTree<Integer>(12);
		twoNodeTree.getRoot().setLeft(new TreeNode<Integer>(56));
		twoNodeTree.getRoot().setRight(new TreeNode<Integer>(77));
	}


}
