#include <bits/stdc++.h> //Remember that this doesn't work on your machine...

using namespace std;

class DoublyLinkedListNode {
    public:
        int data;
        DoublyLinkedListNode *next;
        DoublyLinkedListNode *prev;

        DoublyLinkedListNode(int node_data) {
            this->data = node_data;
            this->next = nullptr;
            this->prev = nullptr;
        }
};

class DoublyLinkedList {
    public:
        DoublyLinkedListNode *head;
        DoublyLinkedListNode *tail;

        DoublyLinkedList() {
            this->head = nullptr;
            this->tail = nullptr;
        }

        void insert_node(int node_data) {
            DoublyLinkedListNode* node = new DoublyLinkedListNode(node_data);

            if (!this->head) {
                this->head = node;
            } else {
                this->tail->next = node;
                node->prev = this->tail;
            }

            this->tail = node;
        }
};

void print_doubly_linked_list(DoublyLinkedListNode* node, string sep, ofstream& fout) {
    while (node) {
        fout << node->data;

        node = node->next;

        if (node) {
            fout << sep;
        }
    }
}

void free_doubly_linked_list(DoublyLinkedListNode* node) {
    while (node) {
        DoublyLinkedListNode* temp = node;
        node = node->next;

        free(temp);
    }
}

// Complete the reverse function below.

/*
 * For your reference:
 *
 * DoublyLinkedListNode {
 *     int data;
 *     DoublyLinkedListNode* next;
 *     DoublyLinkedListNode* prev;
 * };
 *
 */

// Reverse Doubly linked list by swapping all the prev, and next pointers.
DoublyLinkedListNode* rev(DoublyLinkedListNode* t){
    DoublyLinkedListNode* n = t->prev; //First we need to swap those of the initial head node.
    swap(n->prev, n->next);
    while(t->next){ //Then we swap until the very last node
        swap(t->prev, t->next);
        t = t->prev;
    }
    swap(t->next, t->prev); //Swap the last node's pointers
    return t; //Return the last node as it is now our first.
}

DoublyLinkedListNode* reverse(DoublyLinkedListNode* head) {
    if(head != nullptr){
        if(head->next != nullptr)
            return rev(head->next);
    } 
    return head;
}

int main()
{
    ofstream fout(getenv("OUTPUT_PATH"));

    int t;
    cin >> t;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    for (int t_itr = 0; t_itr < t; t_itr++) {
        DoublyLinkedList* llist = new DoublyLinkedList();

        int llist_count;
        cin >> llist_count;
        cin.ignore(numeric_limits<streamsize>::max(), '\n');

        for (int i = 0; i < llist_count; i++) {
            int llist_item;
            cin >> llist_item;
            cin.ignore(numeric_limits<streamsize>::max(), '\n');

            llist->insert_node(llist_item);
        }

        DoublyLinkedListNode* llist1 = reverse(llist->head);

        print_doubly_linked_list(llist1, " ", fout);
        fout << "\n";

        free_doubly_linked_list(llist1);
    }

    fout.close();

    return 0;
}

