<template>
  <page-skeleton>
    <template #breadcrumbs>
      <el-breadcrumb-item :to="{ name: 'recipe-list' }">
        My Recipes
      </el-breadcrumb-item>
      <el-breadcrumb-item>
        {{ recipe.name }}
      </el-breadcrumb-item>
    </template>
    <template #header-right>
      <el-button
        @click="goToEdit"
        size="large"
        type="primary">Edit</el-button>
    </template>
    <h1 class="title">{{recipe.name}}</h1>
    <el-descriptions
      class="recipe-desc"
      border>
      <el-descriptions-item class-name="tag-content" label-class-name="tag-label" label="Tags">
        {{tagString}}
      </el-descriptions-item>
    </el-descriptions>
    <div class="image-list">
      <el-row v-for="url in recipe.images" :key="url">
        <el-col
          :span="24"
          :md="{span: 18, offset: 3}"
          :lg="{span: 14, offset: 5}">
          <el-image
            fit="fill"
            :src="url" />
        </el-col>
      </el-row>
    </div>
    <el-collapse class="ocr-content">
      <el-collapse-item title="Content">
        <p v-for="(text, i) in paragraphs" :key="i">{{text}}</p>
      </el-collapse-item>
    </el-collapse>
  </page-skeleton>
</template>

<script>
import PageSkeleton from "./PageSkeleton.vue"
import { getRecipe } from "../Api.js"

export default {
  name: 'RecipeView',
  components: {PageSkeleton},
  data() {
    return {
      recipe: {
        name: "",
        tags: [],
        images: [],
        body: "",
      },
    }
  },
  computed: {
    paragraphs() {
      return this.$data.recipe.body
        .split(/\n+/g)
        .filter(str => str != "")
    },
    tagString() {
      return this.$data.recipe.tags.join(", ")
    },
  },
  methods: {
    goToEdit() {
      this.$router.push({ name: "recipe-edit" })
    },
  },
  created() {
    getRecipe(this.$route.params.id)
      .then(recipe => {
        this.$data.recipe = recipe
      })
  },
}
</script>

<style>
.title {

}

.recipe-desc {
  margin-bottom: 24px;
}

.tag-label {
  width: 10%;
  font-size: 18px !important;
}

.tag-content {
  font-size: 18px !important;
}

.ocr-content {
  --el-collapse-header-font-size: var(--el-font-size-large);
  --el-collapse-content-font-size: 16px;

}
</style>
