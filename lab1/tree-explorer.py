from conllu import parse
import argparse

parser = argparse.ArgumentParser(description='Returns the counts of upos or xpos tags in a conllu file.')
parser.add_argument('filename', help='The name or the path of the file you want to get the data from.')
parser.add_argument('count', type=int, help='The top X tags to be displayed.')
parser.add_argument('--upos', action='store_true', help='Retrieve upos tag counts.')
parser.add_argument('--xpos', action='store_true', help='Retrieve xpos tag counts.')
args = parser.parse_args()

def read_file(filename):
    with open(filename,'r',encoding = 'utf-8') as f:
        data = f.read()
    sentences = parse(data)

    return sentences

def count_upos(sentences):
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

def top_x(sorted_list, count):
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


if __name__ == "__main__":
    filename = args.filename
    count = args.count
    sentences = read_file(filename)

    if args.xpos:
        print(f'Retrieving top {count} xpos counts from {filename}:')
        sorted_list = count_xpos(sentences)
        top_x(sorted_list, count)

    if args.upos:
        print(f'Retrieving top {count} upos counts from {filename}:')
        sorted_list = count_upos(sentences)
        top_x(sorted_list, count)
