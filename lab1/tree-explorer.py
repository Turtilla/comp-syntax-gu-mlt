from conllu import parse
import argparse

parser = argparse.ArgumentParser(description='Returns the counts of upos or xpos tags in a conllu file.')
parser.add_argument('filename', help='The name or the path of the file you want to get the data from.')
parser.add_argument('--count', type=int, help='The top X tags to be displayed.')
parser.add_argument('--upos', action='store_true', help='Retrieve upos tag counts.')
parser.add_argument('--xpos', action='store_true', help='Retrieve xpos tag counts.')
parser.add_argument('--deprel', action='store_true', help='Retrieve deprel tag counts.')
parser.add_argument('--feats', action='store_true', help='Retrieve features tag counts.')
parser.add_argument('--indi_feats', action='store_true', help='Retrieve individual features tag counts.')
args = parser.parse_args()

def read_file(filename):
    '''Reads the data from the .conllu file and parses it using the conllu module. Returns a list of conllu objects.'''
    if filename.endswith(".conllu"):
        with open(filename,'r',encoding = 'utf-8') as f:
            data = f.read()
        sentences = parse(data)
    else:
        print("This type of file is not allowed!")
        quit()

    return sentences

def count_upos(sentences):
    '''Counts the upos tags in the conllu sentences. Returns a list sorted from the largest item.'''
    count_dictionary = {}
    for sentence in sentences:
        for token in sentence:
            pos_tag = token['upos']

            if pos_tag not in count_dictionary:
                count_dictionary[pos_tag] = 1
            else:
                count_dictionary[pos_tag] += 1

    count_list = []
    for k,v in count_dictionary.items():
        pair = (k,v)
        count_list.append(pair)

    sorted_list = sorted(count_list, reverse=True, key=lambda x: x[1])
    
    return sorted_list

def count_xpos(sentences):
    '''Counts the xpos tags in the conllu sentences. Returns a list sorted from the largest item.'''
    count_dictionary = {}
    for sentence in sentences:
        for token in sentence:
            pos_tag = token['xpos']

            if pos_tag not in count_dictionary:
                count_dictionary[pos_tag] = 1
            else:
                count_dictionary[pos_tag] += 1

    count_list = []
    for k,v in count_dictionary.items():
        pair = (k,v)
        count_list.append(pair)

    sorted_list = sorted(count_list, reverse=True, key=lambda x: x[1])
    
    return sorted_list

def count_deprel(sentences):
    '''Counts the deprel tags in the conllu sentences. Returns a list sorted from the largest item.'''
    count_dictionary = {}
    for sentence in sentences:
        for token in sentence:
            pos_tag = token['deprel']

            if pos_tag not in count_dictionary:
                count_dictionary[pos_tag] = 1
            else:
                count_dictionary[pos_tag] += 1

    count_list = []
    for k,v in count_dictionary.items():
        pair = (k,v)
        count_list.append(pair)

    sorted_list = sorted(count_list, reverse=True, key=lambda x: x[1])
    
    return sorted_list

def count_feats(sentences):
    '''Counts the feats tags in the conllu sentences. Returns a list sorted from the largest item.'''
    count_dictionary = {}
    for sentence in sentences:
        for token in sentence:
            pos_tag = token['feats']
            full_tag = []
            if hasattr(pos_tag, '__len__'):
                for k,v in pos_tag.items():
                    pair = f'{k}={v}'
                    full_tag.append(pair)
                full_tag = tuple(full_tag)
                if full_tag not in count_dictionary:
                    count_dictionary[full_tag] = 1
                else:
                    count_dictionary[full_tag] += 1
            else:
                continue

    count_list = []
    for k,v in count_dictionary.items():
        pair = (k,v)
        count_list.append(pair)

    sorted_list = sorted(count_list, reverse=True, key=lambda x: x[1])
    
    return sorted_list

def count_individual_feats(sentences):
    '''Counts the feats tags in the conllu sentences. Returns a list sorted from the largest item.'''
    count_dictionary = {}
    for sentence in sentences:
        for token in sentence:
            pos_tag = token['feats']
            if hasattr(pos_tag, '__len__'):
                for k,v in pos_tag.items():
                    pair = f'{k}={v}'
                    if pair not in count_dictionary:
                        count_dictionary[pair] = 1
                    else:
                        count_dictionary[pair] += 1
            else:
                pass

    count_list = []
    for k,v in count_dictionary.items():
        pair = (k,v)
        count_list.append(pair)

    sorted_list = sorted(count_list, reverse=True, key=lambda x: x[1])
    
    return sorted_list

def top_x(sorted_list, count):
    '''Prints the top X elements from a list of tuples.'''
    if len(sorted_list) < count:
        print(f'This list has less than {count} entries!')
        for i in range(0,len(sorted_list)):
            k, v = sorted_list[i]
            ranking = str(i+1)
            print(f'{ranking}. {k}: {v}')
    else:
        for i in range(0,count):
            k, v = sorted_list[i]
            ranking = str(i+1)
            print(f'{ranking}. {k}: {v}')
    print('')

def print_all(sorted_list):
    '''Prints all the elements from a list of tuples.'''
    for i in range(0,len(sorted_list)):
        k, v = sorted_list[i]
        ranking = str(i+1)
        print(f'{ranking}. {k}: {v}')

    print('')


if __name__ == "__main__":
    filename = args.filename
    sentences = read_file(filename)

    if args.xpos:
        if args.count:
            count = args.count
            print(f'Retrieving top {count} xpos counts from {filename}:')
            sorted_list = count_xpos(sentences)
            top_x(sorted_list, count)
        else:
            print(f'Retrieving all xpos counts from {filename}:')
            sorted_list = count_xpos(sentences)
            print_all(sorted_list)
            

    if args.upos:
        if args.count:
            count = args.count
            print(f'Retrieving top {count} upos counts from {filename}:')
            sorted_list = count_upos(sentences)
            top_x(sorted_list, count)
        else:
            print(f'Retrieving all upos counts from {filename}:')
            sorted_list = count_upos(sentences)
            print_all(sorted_list) 
        
    if args.deprel:
        if args.count:
            count = args.count
            print(f'Retrieving top {count} deprel counts from {filename}:')
            sorted_list = count_deprel(sentences)
            top_x(sorted_list, count)
        else:
            print(f'Retrieving all deprel counts from {filename}:')
            sorted_list = count_deprel(sentences)
            print_all(sorted_list)

    if args.feats:
        if args.count:
            count = args.count
            print(f'Retrieving top {count} feats counts from {filename}:')
            sorted_list = count_feats(sentences)
            top_x(sorted_list, count)
        else:
            print(f'Retrieving all feats counts from {filename}:')
            sorted_list = count_feats(sentences)
            print_all(sorted_list)

    if args.indi_feats:
        if args.count:
            count = args.count
            print(f'Retrieving top {count} individual feats counts from {filename}:')
            sorted_list = count_individual_feats(sentences)
            top_x(sorted_list, count)
        else:
            print(f'Retrieving all individual feats counts from {filename}:')
            sorted_list = count_individual_feats(sentences)
            print_all(sorted_list)
