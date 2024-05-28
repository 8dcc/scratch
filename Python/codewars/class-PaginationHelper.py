class PaginationHelper:
    def __init__(self, collection, items_per_page):
        self.collection = collection
        self.items_per_page = items_per_page

    def item_count(self):
        return len(self.collection)

    def page_count(self):
        return int(len(self.collection)/self.items_per_page) + 1 if len(self.collection)%self.items_per_page != 0 else int(len(self.collection)/self.items_per_page)

    def page_item_count(self, page_index):
        if page_index+1 > self.page_count():
            return -1
        elif page_index+1 < self.page_count():
            return self.items_per_page
        return -1 if page_index+1 == self.page_count() and len(self.collection)%self.items_per_page == 0 else int(len(self.collection)%self.items_per_page)

    def page_index(self, item_index):
        return int(item_index/self.items_per_page) if item_index >= 0 and item_index < len(self.collection) else -1
